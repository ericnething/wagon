{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Main where

-- Pipes
import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as P (stdin)
import           Pipes.Csv

-- Generics
import           GHC.Generics (Generic)

-- Base
import qualified Data.List as L
import           Data.Monoid
import           Control.Applicative

-- Text
import qualified Data.Text as T
import           Data.Text (Text)

-- | A row of raw data
data Row = Row
           { _sessionId  :: !Text
           , _page       :: !(Maybe Text)
           , _latency    :: !(Maybe Double)
           , _timeOnPage :: !(Maybe Double)
           } deriving (Show, Generic)

instance FromRecord Row

-- | Summary statistics for a row
data RowStats = RowStats
                { _sessionIdStats  :: !Stats
                , _pageStats       :: !Stats
                , _latencyStats    :: !Stats
                , _timeOnPageStats :: !Stats
                }

instance Show RowStats where
  show x = L.unlines
           [ "Session Id:\n"   <> show (_sessionIdStats x)
           , "Page:\n"         <> show (_pageStats x)
           , "Latency:\n"      <> show (_latencyStats x)
           , "Time On Page:\n" <> show (_timeOnPageStats x)
           ]

instance Monoid RowStats where
  mempty = RowStats mempty mempty mempty mempty
  RowStats !a1 !b1 !c1 !d1 `mappend` (RowStats !a2 !b2 !c2 !d2)
    = RowStats (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

-- | Summary statistics for a column
data Stats = Stats
             { _count     :: !Int
             , _nullCount :: !Int
             , _colMin    :: !Double
             , _colMax    :: !Double
             , _colMean   :: !Mean
             }

instance Show Stats where
  show x = L.unlines $ fmap ("    " ++)
           [ "Count:   " <> show (_count x)
           , "Null:    " <> show (_nullCount x)
           , "Minimum: " <> show (_colMin x)
           , "Maximum: " <> show (_colMax x)
           , "Average: " <> show (_colMean x)
           ]

instance Monoid Stats where
  mempty = Stats 0 0 (1/0) (-1/0) mempty
  Stats !a1 !b1 !c1 !d1 !e1 `mappend` (Stats !a2 !b2 !c2 !d2 !e2)
    = Stats (a1 + a2) (b1 + b2) (min c1 c2) (max d1 d2) (e1 <> e2)

-- | Mean value stored as a sum and count
data Mean = Mean !Double !Int

instance Monoid Mean where
  mempty = Mean 0.0 0
  Mean !t1 !n1 `mappend` (Mean !t2 !n2) = Mean (t1 + t2) (n1 + n2)

instance Show Mean where
  show (Mean !t !n)= show (t / (fromIntegral n))

-- | Produce statistics for a given 'Row'
toStats :: Row -> RowStats
toStats Row {..} = RowStats
                  (textColumn (pure _sessionId))
                  (textColumn       _page)
                  (numberColumn     _latency)
                  (numberColumn     _timeOnPage)

-- | Convert Double into a default Column
numberColumn :: Maybe Double -> Stats
numberColumn (Just !x) =
  mempty { _count   = 1
         , _colMin  = x
         , _colMax  = x
         , _colMean = Mean x 1
         }
numberColumn Nothing = nullColumn

-- | Convert Text into a default Column
textColumn :: Maybe Text -> Stats
textColumn (Just !x) =
  mempty { _count   = 1
         , _colMin  = fromIntegral (T.length x)
         , _colMax  = fromIntegral (T.length x)
         , _colMean = Mean (fromIntegral (T.length x)) 1
         }
textColumn Nothing = nullColumn

-- | The Null Column
nullColumn :: Stats
nullColumn = mempty { _nullCount = 1 }


-- | Streaming pipeline from ByteString to RowStats
pipeline :: Producer RowStats IO ()
pipeline = P.map toStats <-< P.concat <-< decode HasHeader P.stdin

main :: IO ()
main = print . fst =<< P.fold' (<>) mempty id pipeline
