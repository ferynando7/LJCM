{-# LANGUAGE OverloadedStrings, ExistentialQuantification, FlexibleContexts, TypeApplications, RecordWildCards #-}

module Main where
--monad-bayes
import Control.Monad
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Class
--hvega
import qualified Graphics.Vega.VegaLite as VL
import qualified Graphics.Vega.Tutorials.VegaLite as VT
--data
import qualified Data.Histogram as DH
import Data.Text (Text, pack)
import Data.List (sort, partition)
import Data.Vector.Unboxed.Base (Unbox)
import qualified Data.Vector.Unboxed as Vec
--numeric
import Numeric.Log
import Numeric.LinearAlgebra (Matrix, Vector, toList, toInt, vector, matrix, dot, (#>), (!), cmap, scalar, size)
--src
import BNN
import PlotVL
--csv
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

--Data Type
data Item =
  Item
    { idNumber :: Double,
      date :: String,
      new_cases :: Double
    }
  deriving (Eq, Show)

--Decoding
instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "idNumber"
      <*> m .: "date"
      <*> m .: "new_cases"

decodeItems :: BL.ByteString -> Either String (Vector Item)
decodeItems = fmap snd . Data.Csv.decodeByName

decodeItemsFromFile :: FilePath -> IO (Either String (Vector Item))
decodeItemsFromFile filePath = catchShowIO (BL.readFile filePath) >>= return . either Left decodeItems

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show

fillingData :: BL.ByteString ->  [Double] -> [Data]
fillingData csvData noise = case decodeByName csvData of
  Left err -> putStrLn err
  Right (_,v) -> [Data (idNumber v) ((new_cases v) + n) | n <- noise]

main :: IO ()
main = do
    -- putStrLn "Enter the number of samples to train: "
    -- input1 <- getLine
    -- let nsamples = (read input1 :: Int)

    putStrLn "Enter the number of nodes: "
    input3 <- getLine
    let nnodes = (read input3 :: Int)

    putStrLn "Enter the number of predicted samples: "
    input4 <- getLine
    let npredictive = (read input4 :: Int)

    let nsamples = 365
    noise <- sampleIOfixed $ replicateM nsamples $ normal 0.0 0.5 

    -- case decodeByName csvData of
    --   Left err -> putStrLn err
    --   Right (_, v) -> decodeItems

    csvData <- BL.readFile "/home/leduin/Desktop/10th Semester/Bayesian Statistics/Project/owid-covid-data-ecuador.csv"
    
    let observations = fillingData csvData noise

    -- let observations = case decodeByName csvData of
    --   Left err -> putStrLn err
    --   Right (_,v) -> [Data (idNumber v) ((new_cases v) + n) | n <- noise]

-- A SIMPLE NEURAL NETWORK
    
    --[ Data x (0.5 * x - 2 + n)
    --let observations = take nsamples
    --         [ Data x (2 * sin (x) + 1 + n)
    --         | (x, n) <- zip (map ((/(fromIntegral nsamples)) . fromIntegral ) [0, 10 ..]) noise
    --         ]

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