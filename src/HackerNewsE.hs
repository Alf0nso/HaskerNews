{-# LANGUAGE OverloadedStrings #-}

module HackerNewsE ( allTag
                   , hackerNewsUrl
                   , hnPostHeader
                   , getPostTitleRank
                   , getPostCommentPoint
                   )
where

import Text.HTML.Scalpel
import Data.List.Split ( splitOn )
import Text.Read

{- Helper functions -}
hackerNewsUrl :: String
hackerNewsUrl = "https://news.ycombinator.com/"

getIntFromHNattr :: String -> String -> (Int, Bool)
getIntFromHNattr s atr =
  case possibleInt of
    Just i  -> (i, True)
    Nothing -> (0, False)
  where
    possibleInt :: Maybe Int
    possibleInt = readMaybe $ head $ splitOn s atr

allTag :: Selector -> URL -> IO (Maybe [String])
allTag s url =
  scrapeURL url (htmls s)

{- Post Title and respective rank. -}
postHeader :: Selector
postHeader = TagString "tr" @: [ hasClass "athing" ]

hnPostHeader :: Scraper String [(Int, String)]
hnPostHeader =  chroots postHeader
                $ do title <- text $ TagString "span" @: [ hasClass
                                                           "titleline" ]
                     rank  <- text $ TagString "span" @: [ hasClass
                                                           "rank" ]
                     return (fst $ getIntFromHNattr "." rank, title)

getPostTitleRank :: IO (Maybe [(Int, String)])
getPostTitleRank = scrapeURL hackerNewsUrl hnPostHeader

{- Post number of comments and respective points. -}
postSubText :: Selector
postSubText = TagString "span" @: [ hasClass "subline" ]

type Points         = Int
type NumberComments = Int

hnPostSubText :: Scraper String [(NumberComments, Points)]
hnPostSubText = chroots postSubText
                $ do points   <- text $ TagString "span" @: [ hasClass
                                                              "score" ]
                     [_, comments] <- texts
                                         $ TagString "a" @:
                                         [ match checkCommentAttr ]

                     return ( fst $ getIntFromHNattr "\160" comments
                            , fst $ getIntFromHNattr " " points)
  where
    checkCommentAttr :: String -> String -> Bool
    checkCommentAttr "href" ('i':'t':'e':'m':_) = True
    checkCommentAttr _ _                        = False

getPostCommentPoint :: IO (Maybe [(NumberComments, Points)])
getPostCommentPoint = scrapeURL hackerNewsUrl hnPostSubText
