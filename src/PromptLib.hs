module PromptLib ( loop ) where

import System.IO
import Text.Read
import HackerNewsE

loop :: [HackerNewsPost] -> [HackerNewsPost] -> IO ()
loop og posts = do input <- prompt "> Enter your commands: "
                   let pInput = handleCommans $ words input
                   case pInput of
                     Quit  -> do putStrLn "Bye :)"
                                 putStrLn ""
                                 return ()

                     Print -> do putStr $ printPosts posts
                                 putStrLn ""
                                 loop og posts

                     Clear -> loop og og

                     Help  -> do putStrLn help
                                 loop og posts

                     Order by -> case by of
                                  Rank     -> loop og $ sortByRank posts
                                  Points   -> loop og
                                              $ sortByPoints posts
                                  Comments -> loop og
                                              $ sortByComments posts

                     Filter f -> loop og $ filterByWordSize f posts

                     NotFound -> do putStrLn notFound
                                    loop og posts

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

data Command = Quit
             | Print
             | Clear
             | Help
             | Filter (Int -> Bool)
             | Order OrderedBy
             | NotFound

data OrderedBy = Rank
               | Points
               | Comments

help :: String
help = "All the commands: \n" ++
       "q | quit       -- quiting the program.\n" ++
       "p | print      -- printing Hacker news posts to the\n"
       ++ "                  terminal.\n"
       ++
       "c | clear      -- Cleans all the changes done to the data\n"
       ++
       "                  returning to the original requested data.\n"
       ++
       "f | filterWith -- Filter data with a specific filter function\n"
       ++ "                  which can be for example: filter > 5.\n"
       ++
       "o | orderBy    -- Order data by Rank, Points or number of\n"
       ++ "                  comments, example: orderBy rank\n" ++
       "h | help       -- prints this message.\n"

notFound :: String
notFound =
  "Command not found, try to write \"h\" or \"help\" for help!"

handleCommans :: [String] -> Command
handleCommans ("help":_)          = Help
handleCommans ("h":_)             = Help
handleCommans ("quit":_)          = Quit
handleCommans ("q":_)             = Quit
handleCommans ("print":_)         = Print
handleCommans ("p":_)             = Print
handleCommans ("clear":_)         = Clear
handleCommans ("c":_)             = Clear
handleCommans ["orderBy"]         = NotFound
handleCommans ["o"]               = NotFound
handleCommans ("orderBy":rest)    = orderedBy rest
handleCommans ("o":rest)          = orderedBy rest
handleCommans ["filterWith"]      = NotFound
handleCommans ["f"]               = NotFound
handleCommans ("filterWith":rest) = filterWith rest
handleCommans ("f":rest)          = filterWith rest
handleCommans _                   = NotFound

filterWith :: [String] -> Command
filterWith [_]        = NotFound
filterWith (">":num:[])  = checkInt (>) num
filterWith (">=":num:[]) = checkInt (>=) num
filterWith ("<":num:[])  = checkInt (<) num
filterWith ("<=":num:[]) = checkInt (<=) num
filterWith _             = NotFound

checkInt :: (Int -> Int -> Bool) -> String -> Command
checkInt f num = case readMaybe num of
                   Just i  -> Filter (f i)
                   Nothing -> NotFound

orderedBy :: [String] -> Command
orderedBy ("Rank":_)     = Order Rank
orderedBy ("rank":_)     = Order Rank
orderedBy ("r":_)        = Order Rank
orderedBy ("Points":_)   = Order Points
orderedBy ("points":_)   = Order Points
orderedBy ("p":_)        = Order Points
orderedBy ("Comments":_) = Order Points
orderedBy ("comments":_) = Order Points
orderedBy ("c":_)        = Order Points
orderedBy _              = NotFound
