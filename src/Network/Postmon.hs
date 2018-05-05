{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, ScopedTypeVariables #-}

module Network.Postmon 
    ( fetchPosts
    ) where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Control.Exception (try)

gateway :: String -> String
gateway = (++) "http://api.postmon.com.br/v1/rastreio/ect/"

data History = History { 
    date :: String
  , local :: String
  , status :: String
  } deriving (Show, Generic)

instance FromJSON History where
  parseJSON = withObject "history" $ \o -> do
    status <- o .: "situacao"
    local  <- o .: "local"
    date <- o .: "data"
    return History{..}

instance ToJSON History where
  toJSON History{..} = object 
    [ "data" .= date
    , "local" .= local
    , "situacao" .= status
    ]

history :: Value -> Parser [History]
history = withObject "history" (.: "historico")

get :: String -> IO (Maybe [History])
get code = do
  response <- try . simpleHttp $ gateway code
  case response of
    Left (e :: HttpException) -> return Nothing
    Right r -> return $ parseMaybe history =<< decode r

statusPost :: History -> String
statusPost (History {status="Objeto entregue ao destinat\239\191\189rio", date=d, local=l}) =
  "Anta Baka?! Your package has been delivered in " ++ l ++ " since " ++ d ++ "!"
statusPost (History {status=s, date=d, local=l}) = "How can *I* know? :rage:"

fetchPosts :: Text -> IO Text
fetchPosts code = do 
  content <- get $ unpack code
  case content of
    Just a -> return . pack . statusPost $ Prelude.last a
    Nothing -> return "Of course I haven't found anything: It-Doesn't-Even-Exist! :rage:"
  