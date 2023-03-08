module Main (main) where

import HackerNewsE

main :: IO ()
main = do
  posts <- getInformationOn
  
  case posts of
    Just ps -> do putStrLn $ printPosts ps
                  print $ length ps
    Nothing  ->
      putStrLn "It was not possible to retrive data from hacker news!"
