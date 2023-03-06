module HackerNewsE ( allTag
                   , hackerNewsUrl
                   , hnPostHeader
                   , getPostTitle
--                   , hackerNewsPathToP
--                   , getPostInfo
--                   , hackerNewsPosts
--                   , allPagePosts )
                   )
where

import Text.HTML.Scalpel

hackerNewsUrl :: String
hackerNewsUrl = "https://news.ycombinator.com/"

allTag :: Selector -> URL -> IO (Maybe [String])
allTag s url =
  scrapeURL url (htmls s)

data HNPost = HNPost { head :: String, subtext :: String }

{- Post Title and respective rank. -}
postHeader :: Selector
postHeader = TagString "tr" @: [ hasClass "athing" ]

hnPostHeader :: Scraper String [(Int, String)]
hnPostHeader =  chroots postHeader
                $ do title <- text $ TagString "span" @: [ hasClass
                                                           "titleline" ]
                     rank  <- text $ TagString "span" @: [ hasClass
                                                           "rank" ]
                     
                     return (rankToInt rank, title)
  where
    rankToInt :: String -> Int
    rankToInt = read . init

getPostTitle :: IO (Maybe [(Int, String)])
getPostTitle = scrapeURL hackerNewsUrl hnPostHeader


{- Post number of comments and respective points. -}
postSubText :: Selector
postSubText = TagString "span" @: [ hasClass "subline" ]

type Points         = Int
type NumberComments = Int
hnPostSubText :: Scraper String [(Int, Points)]
hnPostSubText = 



-- head :: Scraper String HNPost
-- head = 
-- headAndSubtext :: Scraper String HNPost
-- headAndSubtext = inSerial $
--   do head    <- text $ TagString "tr" @: [ hasClass "athing" ]
--      subtext <- text $ TagString "td" @: [ hasClass "subtext" ]
--      return $ HNPost head subtext
-- 
