module Main (main) where

import HackerNewsE
import Text.HTML.Scalpel

-- main :: IO ()
-- main = do
--   something <- allTag hackerNewsPathToP hackerNewsUrl
--   case something of
--     Just s -> do print s
--                  print $ length s
--                  
--     Nothing -> print "no element"

-- main :: IO ()
-- main = do
--   something <- allTag test hackerNewsUrl
--   case something of
--     Just s -> print s
--     Nothing -> print "no element"

main :: IO ()
main = do
  posts <- getPostCommentPoint
  case posts of
    Just ps -> do print ps
                  print $ length ps
    Nothing  -> print "error"
