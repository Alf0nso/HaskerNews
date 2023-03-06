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
import Control.Applicative (many)

hackerNewsUrl :: String
hackerNewsUrl = "https://news.ycombinator.com/"

allTag :: Selector -> URL -> IO (Maybe [String])
allTag s url =
  scrapeURL url (htmls s)

-- allPagePosts :: IO (Maybe [HNPost])
-- allPagePosts = scrapeURL hackerNewsUrl hackerNewsPosts

data HNPost = HNPost { head :: String, subtext :: String }

postHeader :: Selector
postHeader = TagString "tr" @: [ hasClass "athing" ]

hnPostHeader :: Scraper String [String]
hnPostHeader = chroots postHeader
               $ do title <- text $ TagString "span" @: [ hasClass
                                                          "titleline" ]
                    return title

getPostTitle :: IO (Maybe [String])
getPostTitle = scrapeURL hackerNewsUrl hnPostHeader
                      
  

postSubText :: Selector
postSubText = TagString "span" @: [ hasClass "subline" ]

-- head :: Scraper String HNPost
-- head = 
-- headAndSubtext :: Scraper String HNPost
-- headAndSubtext = inSerial $
--   do head    <- text $ TagString "tr" @: [ hasClass "athing" ]
--      subtext <- text $ TagString "td" @: [ hasClass "subtext" ]
--      return $ HNPost head subtext
-- 
