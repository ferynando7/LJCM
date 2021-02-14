{-# LANGUAGE OverloadedStrings, ExistentialQuantification, FlexibleContexts, TypeApplications #-}

module Main where

import Control.Monad
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Class

import qualified Graphics.Vega.VegaLite as VL
import qualified Graphics.Vega.Tutorials.VegaLite as VT

import qualified Data.Histogram as DH
import Data.Text (Text, pack)
import Data.List (sort, partition)
import Data.Vector.Unboxed.Base (Unbox)
import qualified Data.Vector.Unboxed as Vec

import Numeric.Log
import Numeric.LinearAlgebra (Matrix, Vector, toList, toInt, vector, matrix, dot, (#>), (!), cmap, scalar, size)

--import FirstSteps
import BNN
import PlotVL

main :: IO ()
main = do
    putStrLn "Enter the number of samples to train: "
    input1 <- getLine
    let nsamples = (read input1 :: Int)

    --putStrLn "Enter the function: "
    --input2 <- getLine
    --let funcTrain = (read input2 :: Double)

    putStrLn "Enter the number of nodes: "
    input3 <- getLine
    let nnodes = (read input3 :: Int)

    putStrLn "Enter the number of predicted samples: "
    input4 <- getLine
    let npredictive = (read input4 :: Int)

-- A SIMPLE NEURAL NETWORK

    --let nsamples = 200
    noise <- sampleIOfixed $ replicateM nsamples $ normal 0.0 0.5
    
    --[ Data x (0.5 * x - 2 + n)
    let observations = take nsamples
            [ Data x (2 * sin (x) + 1 + n)
            | (x, n) <- zip (map ((/(fromIntegral nsamples)) . fromIntegral ) [0, 10 ..]) noise
            ]

        --nnodes = 3
        --mkSampler = prior . mh 60000
        mkSampler = prior . mh npredictive
    predicted <-
        sampleIOfixed $ mkSampler $ predDist $
        postNN (priorNN nnodes) observations

    let hist = histo2D (0, 10, 10) (-10, 20, 10)
                ((\(nn, d) -> (xValue d, yValue d)) <$> predicted)
        cents = Vec.toList $ DH.binsCenters $ DH.bins hist
        val = Vec.toList $ DH.histData hist

    VL.toHtmlFile "final.html" $
        plot
            (600, 300)
            (L [imagePlot "xModel" "yModel" "zModel", scatterBlue "xObs" "yObs" (0, 10) (-10, 10)])
            ( Cols
                [ ("xModel", VL.Numbers (fst <$> cents)),
                ("yModel", VL.Numbers (snd <$> cents)),
                ("zModel", VL.Numbers val),
                ("xObs", VL.Numbers (xValue <$> observations)),
                ("yObs", VL.Numbers (yValue <$> observations))
                ]
            )

    putStrLn "Success! Check html images in directory"