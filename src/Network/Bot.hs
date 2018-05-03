{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Network.Bot
    ( runExample
    ) where

import Network.Postmon
import Data.Text
import Data.Text.Manipulate as L
import Pipes
import System.IO.Unsafe (unsafePerformIO)

import Network.Discord

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

replyPost :: Text -> IO String
replyPost code = do 
  let str = L.dropWord code
  posts <- fetchPosts $ unpack str
  case posts of
      Just a -> return $ show a
      Nothing -> return "Nothing found actually"

runExample :: IO ()
runExample = runBot (Bot "Token") $ do
  with ReadyEvent $ \(Init v u _ _ _) ->
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("baka!" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
      liftIO (replyPost messageContent) >>= reply msg . pack
      