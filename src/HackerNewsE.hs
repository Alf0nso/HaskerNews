module HackerNewsE ( allTag
                   , hackerNewsUrl
                   , hnPostHeader
                   , getPostTitleRank
                   , getPostCommentPoint
                   , getInformationOn
                   , printPosts )
where

import Text.HTML.Scalpel
import Data.List.Split ( splitOn )
import Text.Read

{- Helper functions -}
type Title          = String
type Points         = Int
type NumberComments = Int
type Rank           = Int
data HackerNewsPost = HackerNewsPost { title    :: Title
                                     , rank     :: Rank
                                     , comments :: NumberComments
                                     , points   :: Points }
                      deriving Show

{- print posts in a more readable way -}
printPosts :: [HackerNewsPost] -> String
printPosts [] = ""
printPosts ((HackerNewsPost title rank comments points):posts) =
  "Title:     " ++ title         ++ "\n" ++
  "Rank:      " ++ show rank     ++ "\n" ++
  "Points:    " ++ show points   ++ "\n" ++
  "Comments:  " ++ show comments ++ "\n" ++
  "----------------------------------\n" ++
  printPosts posts

combineAll :: [(Rank, Title)]
           -> [(NumberComments, Points)]
           -> [HackerNewsPost]
combineAll [] [] = []
combineAll ((rank,title):trs) ((comments, points):cps) =
  (HackerNewsPost title rank comments points):combineAll trs cps

{- The important url -}
hackerNewsUrl :: String
hackerNewsUrl = "https://news.ycombinator.com/"

{- If false it's because it was not possible
to convert into an integer -}
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

getPostTitleRank :: IO (Maybe [(Rank, Title)])
getPostTitleRank = scrapeURL hackerNewsUrl hnPostHeader

{- Post number of comments and respective points. -}
postSubText :: Selector
postSubText = TagString "span" @: [ hasClass "subline" ]

hnPostSubText :: Scraper String [(NumberComments, Points)]
hnPostSubText = chroots postSubText
                $ do points        <- text
                                      $ TagString "span" @: [ hasClass
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

{- Combining all the information from both scrappings -}
getInformationOn :: IO (Maybe [HackerNewsPost])
getInformationOn = do ranksTitles    <- getPostTitleRank
                      commentsPoints <- getPostCommentPoint

                      case ranksTitles of
                        Nothing -> return Nothing
                        Just rt -> case commentsPoints of
                                    Nothing -> return Nothing
                                    Just cp ->
                                      return $ Just $ combineAll rt cp
