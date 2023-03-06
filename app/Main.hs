module Main (main) where

import HackerNewsE
import Text.HTML.Scalpel

-- main :: IO ()
-- main = do
--   something <- allTag hackerNewsPosts hackerNewsUrl
--   case something of
--     Just s -> print s
--     Nothing -> print "no element"

-- main :: IO ()
-- main = do
--   something <- allTag test hackerNewsUrl
--   case something of
--     Just s -> print s
--     Nothing -> print "no element"

main :: IO ()
main = do
  posts <- allPagePosts
  case posts of
    Just ps -> print ps
    Nothing  -> print "error"
