module Main where

import Control.Monad.Bayes.Sampler
import Control.Monad
import qualified Graphics.Vega.VegaLite as VL
import qualified Graphics.Vega.Tutorials.VegaLite as VT
import Data.Text (Text, pack)
import FirstSteps



-- FirstSteps
-- nsamples = 1000
-- samples <- sampleIOfixed $ replicateM nsamples model1
-- vlShow $ plot (200, 100) (L [barPlot (pack "b")]) (Cols [((pack "b"), VL.Booleans samples)])

main :: IO ()
main = do
    nsamples <- return 1000
    samples <- sampleIOfixed $ replicateM nsamples model1
    VL.toHtmlFile "histo.html" (plot (200, 100) (L [barPlot (pack "b")]) (Cols [((pack "b"), VL.Booleans samples)]))
    
    
    
    putStrLn "Light Emitting Diode"
