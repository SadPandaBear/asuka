{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Network.Postmon 
    ( fetchPosts
    ) where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics

gateway :: String -> String
gateway = (++) "http://api.postmon.com.br/v1/rastreio/ect/"

data History = History { 
    date :: String
  , local :: String
  , situacao :: String
  } deriving (Show, Generic)

instance FromJSON History where
  parseJSON = withObject "history" $ \o -> do
    situacao <- o .: "situacao"
    local  <- o .: "local"
    date <- o .: "data"
    return History{..}

instance ToJSON History where
  toJSON History{..} = object 
    [ "data" .= date
    , "local" .= local
    , "situacao" .= situacao
    ]

history :: Value -> Parser [History]
history = withObject "history" (.: "historico")

get :: String -> IO (Maybe [History])
get code = do
  response <- simpleHttp $ gateway code
  return $ parseMaybe history =<< decode response

statusPost :: History -> String
statusPost (History {situacao="Objeto entregue ao destinat\239\191\189rio", date=d, local=l}) =
  "Anta Baka?! Your package has been delivered in " ++ l ++ " since " ++ d ++ "!"
statusPost (History {situacao=s, date=d, local=l}) = "How can *I* know?"

fetchPosts :: Text -> IO Text
fetchPosts code = do 
  content <- get $ unpack code
  case content of
    Just a -> return . pack . statusPost $ Prelude.last a
    Nothing -> return "Nothing found actually"
  