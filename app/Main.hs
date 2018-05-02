module Main where

import Network.Postmon 
import Network.Bot

main :: IO ()
main = do 
  posts <- fetchPosts "JT498565583BR"
  putStrLn posts
