{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import WordFrequency

{- Datastructures to capture and generate the JSON structures for the request body of the REST API -}
data FreqForWord = FreqForWord
  { text :: T.Text,
    word :: T.Text
  }
  deriving (Generic, FromJSON)

newtype HighestFreq = HighestFreq
  { text :: T.Text
  }
  deriving (Generic, FromJSON)

data MostFrequentWords = MostFrequentWords
  { n :: Int,
    text :: T.Text
  }
  deriving (Generic, FromJSON)

type API =
  "calculateFrequencyForWord" :> ReqBody '[JSON] FreqForWord :> Post '[JSON] Int
    :<|> "calculateHighestFrequency" :> ReqBody '[JSON] HighestFreq :> Post '[JSON] Int
    :<|> "calculateMostFrequentNWords" :> ReqBody '[JSON] MostFrequentWords :> Post '[JSON] [WordFrequency]

api :: Proxy API
api = Proxy

server :: Server API
server =
  (\FreqForWord {..} -> return (calculateFrequencyForWord text word))
    :<|> (\HighestFreq {..} -> return (calculateHighestFrequency text))
    :<|> (\MostFrequentWords {..} -> return (calculateMostFrequentNWords text n))

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
