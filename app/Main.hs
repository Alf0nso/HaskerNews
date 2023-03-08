module Main (main) where

import HackerNewsE
import PromptLib

main :: IO ()
main = do
  posts <- getInformationOn
  case posts of
    Just ps -> do loop ps ps
    Nothing  ->
      putStrLn "It was not possible to retrive data from hacker news!"
