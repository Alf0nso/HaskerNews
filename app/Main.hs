module Main (main) where

import HackerNewsE

main :: IO ()
main = do
  posts <- getInformationOn
  case posts of
    Just ps -> do putStrLn $ printPosts
                   $ filterByWordSize ((>) 5) ps
                  print $ length $ filterByWordSize ((>) 5) ps
    Nothing  ->
      putStrLn "It was not possible to retrive data from hacker news!"

-- loop :: [HackerNewsPost] -> IO ()
-- loop posts = do
