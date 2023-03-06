module Main (main) where

import Text.HTML.Scalpel

main :: IO ()
main = do
  something <- everything
  case something of
    Just s -> print s
    nothing -> print "upssss"

everything :: IO (Maybe [String])
everything =
  scrapeURL "https://news.ycombinator.com/" (htmls (tagSelector "tr"))

