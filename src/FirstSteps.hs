module FirstSteps where

import System.Random.MWC (createSystemRandom)
import qualified Graphics.Vega.VegaLite as VL
--import Data.Aeson (ToJSON(toJSON), Value)
--import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (sort, partition)

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

import Numeric.Log

-- MODELS FOR FIRST STEPS

model1 :: MonadSample m => m Bool
model1 = do
    b <- uniformD [False, True]
    return b

model2 :: MonadInfer m => m Bool
model2 = do
    b <- uniformD [False, True]
    score (if b then 1.0 else 0.0)
    return b

model3 :: MonadInfer m => m (Double, Double)
model3 = do
    b <- uniform (-1) 1
    m <- uniform (-1) 1
    condition $ (b-m) > 0
    return (b, m)

model4 :: MonadInfer m => m (Double, Double)
model4 = do
    b <- uniform (-1) 1
    m <- uniform (-1) 1
    condition $ (b-m) > 0
    condition $ (b+m) > 0
    return (b, m)

-- FOR LINEAR REGRESSION
-- Model Setup
data Data
  = Data
      { xValue :: Double,
        yValue :: Double
      }
  deriving (Eq, Show)

data Params
  = Params
      { slope :: Double,
        intercept :: Double,
        noiseStd :: Double
      }
  deriving (Eq, Show)

likelihood :: Params -> Data -> Log Double
likelihood (Params m b nStd) (Data x y) = normalPdf (m*x + b) nStd y

-- The Model As A Family Of Distributions

params0 = Params {slope=2.3, intercept=(-1.2), noiseStd=2.0}

samplingDistribution' :: Data -> Double
samplingDistribution' = exp . ln . likelihood params0

priorParams :: MonadSample m => m Params
priorParams = do
  intercept <- uniform (-5) 5
  slope <- uniform (-5) 5
  noise <- uniform 1 3
  return $ Params slope intercept noise

-- Generating Data - MCMC

likelihoodDist :: MonadInfer m => Params -> m Data
likelihoodDist params = do
  x <- uniform (-10) 10
  y <- uniform (-10) 10
  score $ likelihood params (Data x y)
  return $ Data x y

-- Generating Data - Rejection Sampling

uniform2D :: MonadSample m => m Data
uniform2D = do
  x <- uniform (-10) 10
  y <- uniform (-10) 10
  return $ Data x y

-- Linear Regression - Inferring Slope and Intercept

postParams :: MonadInfer m => m Params -> [Data] -> m Params
postParams pr obs = do
  param <- pr
  forM_ obs (\point -> score (likelihood param point))
  return param

-- Simulating Data

predDist :: MonadInfer m => m Params -> m Data
predDist paramDist = do
  params <- paramDist
  point <- likelihoodDist params
  return point