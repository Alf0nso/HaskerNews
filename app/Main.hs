module Main (main) where

import HackerNewsE
import System.IO

main :: IO ()
main = do
  posts <- getInformationOn
  case posts of
    Just ps -> do loop ps
    Nothing  ->
      putStrLn "It was not possible to retrive data from hacker news!"

loop :: [HackerNewsPost] -> IO ()
loop posts = do input <- prompt "> Enter your commands: "
                let pInput = handleCommans $ words input
                
                case pInput of
                  Quit  -> do putStrLn "Bye :)"
                              return ()
                  Print -> do putStr $ printPosts posts
                              loop posts
                  Help  -> do putStrLn "All the commands are..."
                              loop posts
                  NotFound -> do putStrLn notFound
                                 loop posts

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

data Command = Quit
             | Print
             | Help
             | Filter
             | Order
             | NotFound

notFound :: String
notFound =
  "Command not found, try to write \"h\" or \"help\" for help!"

handleCommans :: [String] -> Command
handleCommans ("quit":rest)  = Quit
handleCommans ("q":rest)     = Quit
handleCommans ("print":rest) = Print
handleCommans ("p":rest)     = Print
handleCommans ("help":rest)  = Help
handleCommans ("h":rest)     = Help
handleCommans _              = NotFound

--putStrLn $ printPosts $ filterByWordSize ((>) 5) ps
--print $ length $ filterByWordSize ((>) 5) ps
