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

replyPost :: Message -> Text -> IO (Effect DiscordM ())
replyPost msg code = do 
  let str = L.dropWord code
  posts <- fetchPosts $ unpack str
  print posts
  case posts of
      Just a -> return $ reply msg $ pack $ show a
      -- TODO: Fix this shit
      Nothing -> return $ reply msg $ pack "Nothing found actually"

runExample :: IO ()
runExample = runBot (Bot "NDQxMDI3NTcyNDk1Njc5NTA5.DczDKw.vB6qmtGyRviCJOQhUYaKOEPWuOc") $ do
  with ReadyEvent $ \(Init v u _ _ _) ->
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("baka!" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
      -- TODO: Fix this shit
      unsafePerformIO $ replyPost msg messageContent
      