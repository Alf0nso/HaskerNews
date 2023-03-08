module HackerNewsE ( getInformationOn
                   , printPosts
                   , filterByWordSize
                   , sortByRank
                   , sortByComments
                   , sortByPoints
                   , allTag )
where

import Text.HTML.Scalpel ( TagName( TagString )
                         , Selector
                         , Scraper
                         , URL
                         , htmls
                         , scrapeURL
                         , (@:)
                         , hasClass
                         , chroots
                         , text
                         , texts
                         , match )
import Data.List       ( sortBy )
import Data.Ord        ( comparing )
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
                      deriving ( Show, Eq, Ord )

{- print posts in a more readable way -}
printPosts :: [HackerNewsPost] -> String
printPosts [] = ""
printPosts ((HackerNewsPost t r c p):posts) =
  "Title:     " ++ t      ++ "\n" ++
  "Rank:      " ++ show r ++ "\n" ++
  "Points:    " ++ show p ++ "\n" ++
  "Comments:  " ++ show c ++ "\n" ++
  "----------------------------------\n" ++
  printPosts posts

combineAll :: [(Rank, Title)]
           -> [(NumberComments, Points)]
           -> [HackerNewsPost]
combineAll [] [] = []
combineAll ((r, t):trs) ((c, p):cps) =
  (HackerNewsPost t r c p):combineAll trs cps
combineAll _ _   = []

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
                $ do t <- text $ TagString "span" @: [ hasClass
                                                       "titleline" ]
                     r <- text $ TagString "span" @: [ hasClass
                                                       "rank" ]
                     return (fst $ getIntFromHNattr "." r, t)

getPostTitleRank :: IO (Maybe [(Rank, Title)])
getPostTitleRank = scrapeURL hackerNewsUrl hnPostHeader

{- Post number of comments and respective points. -}
postSubText :: Selector
postSubText = TagString "span" @: [ hasClass "subline" ]

hnPostSubText :: Scraper String [(NumberComments, Points)]
hnPostSubText = chroots postSubText
                $ do p        <- text $ TagString "span" @: [ hasClass
                                                              "score" ]
                     [_, c] <- texts $ TagString "a" @:
                               [ match checkCommentAttr ]

                     return ( fst $ getIntFromHNattr "\160" c
                            , fst $ getIntFromHNattr " " p)
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

{- Filter the HackerNews Posts by word size -}
filterByWordSize :: (Int -> Bool) -> [HackerNewsPost] -> [HackerNewsPost]
filterByWordSize _ [] = []
filterByWordSize func (post@(HackerNewsPost {title = t}):posts)
  | func $ length titleWords = filterByWordSize func posts
  | otherwise                = post:filterByWordSize func posts
  where
    titleWords :: [String]
    titleWords = words t

{- Order list of HackerNews Posts -}
sortByRank :: [HackerNewsPost] -> [HackerNewsPost]
sortByRank = sortBy (comparing rank)

sortByComments :: [HackerNewsPost] -> [HackerNewsPost]
sortByComments = sortBy (comparing comments)

sortByPoints :: [HackerNewsPost] -> [HackerNewsPost]
sortByPoints = sortBy (comparing points)
