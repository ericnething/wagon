{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

-- WebSockets
import           Network.WebSockets

-- Monad
import           Control.Monad
import           Control.Monad.Reader

-- Text
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)

-- IO
import           System.IO (Handle, IOMode(..), withFile)

-- JSON
import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)

-- Pipes
import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Csv (decode, HasHeader(..))
import           Pipes.ByteString (fromHandle)


-- | A row from a CSV file
data Row = Row { _row :: [Text] } deriving (Generic, Show)
instance ToJSON Row

-- | Application with global configuration
type Stream = ReaderT Net IO

-- | Global configuration
newtype Net = Net { _conn :: Connection }

main :: IO ()
main = runServer "127.0.0.1" 8080 app

-- | Accept a connection and run the stream
app :: ServerApp
app pending = runReaderT stream . Net =<< acceptRequest pending

-- | Maximum number of rows to send for each request
chunkSize :: Int
chunkSize = 300

-- | Stream of data
stream :: Stream ()
stream = do
  conn <- asks _conn
  let process h = runEffect $ sendRow conn <-< awaitMsg chunkSize conn <-< rows h
  liftIO $ withFile "input.csv" ReadMode process

-- | Produces a stream of Rows from a CSV file
rows :: Handle -> Producer Row IO ()
rows h = P.map Row <-< P.concat <-< decode NoHeader (fromHandle h)

-- | Send out the next item
sendRow :: Connection -> Consumer Row IO ()
sendRow conn = await >>= liftIO . sendTextData conn . JS.encode >> sendRow conn

-- | Wait for a message before sending items downstream
awaitMsg :: Int -> Connection -> Pipe Row Row IO ()
awaitMsg n conn = liftIO (receiveDataMessage conn) >>
                  replicateM_ n (await >>= yield) >>
                  awaitMsg n conn
