{-# LANGUAGE OverloadedStrings, ExistentialQuantification, FlexibleContexts #-}

module BNN where

import System.Random.MWC (createSystemRandom)

--import Data.Aeson (ToJSON(toJSON), Value)
--import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (sort, partition)
import qualified Data.Histogram as DH
import qualified Data.Histogram.Fill as DF
import qualified Data.Histogram.Bin.Bin2D as Bin2D
import Data.Vector.Unboxed.Base (Unbox)
import qualified Data.Vector.Unboxed as Vec

import Control.Monad (liftM2, replicateM, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Inference.SMC as SMC
import Control.Monad.Bayes.Inference.RMSMC as RMSMC
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Traced.Static (Traced)
import Numeric.LinearAlgebra (Matrix, Vector, toList, vector, matrix, dot, (#>), (!), cmap, scalar, size)

import Numeric.Log

-- Plotting

histo2D :: (Foldable f, Unbox val, Num val) =>
    (Double, Int, Double)
    -> (Double, Int, Double)
    -> f (Double, Double)
    -> DH.Histogram (Bin2D.Bin2D DH.BinD DH.BinD) val
histo2D (xmin, xn, xmax) (ymin, yn, ymax) = DF.fillBuilder buildr
  where
    binX = DH.binD xmin xn xmax
    binY = DH.binD ymin yn ymax
    bins = Bin2D.Bin2D binX binY
    buildr = DF.mkSimple bins
    
histo :: (Foldable v, Unbox a, Num a) =>
         (Double, Int, Double)
      -> v Double
      -> DH.Histogram DF.BinD a
histo (xmin, n, xmax) = DF.fillBuilder buildr
  where
    bins = DH.binD xmin n xmax
    buildr = DF.mkSimple bins

-- A SIMPLE NEURAL NETWORK
-- Model Setup

data NN
  = NN
      { biass :: Vector Double,
        weights :: Vector Double,
        sigma :: Double
      }
  deriving (Eq, Show)

data Data
  = Data
      { xValue :: Double,
        yValue :: Double
      }
  deriving (Eq, Show)

forwardNN :: NN -> Double -> Double
forwardNN (NN bs ws _) x =
    ws `dot` cmap activation (scalar x - bs)
  where activation x = if x < 0 then 0 else 1

errorModel :: Double -> Double -> Double -> Log Double
errorModel mean std = normalPdf mean std

likelihood :: NN -> Data -> Log Double  
likelihood nn (Data xObs yObs) =
    errorModel yMean ySigma yObs
  where
    ySigma = sigma nn
    yMean = forwardNN nn xObs

-- Prior, Posterior and Predictive Distribution

postNN :: MonadInfer m => m NN -> [Data] ->  m NN
postNN pr obs = do
  nn <- pr
  forM_ obs (score . likelihood nn)
  return nn

uniformVec :: MonadSample m => (Double, Double) -> Int -> m (Vector Double)
uniformVec (wmin, wmax) nelements =
  vector <$> replicateM nelements (uniform wmin wmax)

priorNN :: MonadSample m => Int -> m NN
priorNN nnodes = do
  bias <- uniformVec (0, 10) nnodes
  weight <- uniformVec (-10, 10) nnodes
  sigma <- uniform 0.5 1.5
  return $ NN bias weight sigma

predDist :: MonadInfer m => m NN -> m (NN, Data)
predDist pr = do
  nn <- pr
  x <- uniform 0 10
  y <- uniform (-5) 10
  score $ likelihood nn (Data x y)
  return (nn, Data x y)
  