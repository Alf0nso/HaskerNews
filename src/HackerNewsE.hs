module HackerNewsE ( allTag
                   , hackerNewsUrl
                   , hackerNewsPosts
                   , allPagePosts )
where

import Text.HTML.Scalpel

hackerNewsUrl :: String
hackerNewsUrl = "https://news.ycombinator.com/"

allTag :: Selector -> URL -> IO (Maybe [String])
allTag s url =
  scrapeURL url (htmls s)

allPagePosts :: IO (Maybe [HNPost])
allPagePosts = scrapeURL hackerNewsUrl hackerNewsPosts

{- The hacker news website contains all the posts on a table.
This table follows the following structure:
<tr><td><table>...</table></td></tr> -}

hackerNewsPathToP :: Selector
hackerNewsPathToP = tagSelector "tr"
                    // tagSelector "td"
                    // tagSelector "table"

data HNPost = HNPost
  { head :: String, subText :: String }
  deriving Show

hackerNewsPosts :: Scraper String [HNPost]
hackerNewsPosts = chroots hackerNewsPathToP headAndSubtext

headAndSubtext :: Scraper String HNPost
headAndSubtext = do
  head    <- text $ TagString "tr" @: [ hasClass "athing" ]
  subtext <- text $ TagString "td" @: [ hasClass "subtext" ]
  return $ HNPost head subtext
