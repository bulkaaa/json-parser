{-|
Module      : Main
Description : A main module containing user interface for the program
Copyright   : Aleksandra Bulka
-}


module Main where

import System.Environment
import Lib

import Data.Text 
import Data.Maybe
import Text.Read
import Control.Monad.State.Strict
import Pipes
import Pipes.Attoparsec
import System.IO

-- |A helper function to parse a humanly-readable path to a node wrom user
getPath :: IO [String]
getPath = do
    input <- getLine
    case parseInput input of 
        Nothing -> return []
        Just path_part -> do
            more_path <- getPath
            return (path_part : more_path)

-- |A helper function for `getPath` checking for end of inputting a path
parseInput :: String -> Maybe String
parseInput input = if input == ":q" then Nothing else (readMaybe input):: Maybe String


-- |A main function for getting the input file name from user, what operation he would like to perform and with which parameters.
main = do
  putStrLn "Please input file name"  
  inFile <- getLine
  withFile inFile  ReadMode  $ \hIn  ->
    withFile "outFile.txt" WriteMode $ \hOut -> do
    putStrLn "Please select what would you like to do: ADD, REMOVE, MODIFY"  
    option <- getLine
    case option of 
      "MODIFY" -> do 
        putStrLn "Enter key to be modified"  
        key <- getLine
        putStrLn "Enter value for this key"  
        val <- getLine
        (runStateT (runEffect $ parsingProducer hIn  >-> valueModifier (Modify key val) >-> bsWriter hOut) ([], Nothing))
      "ADD" -> do 
        putStrLn "Enter key to be added"  
        key <- getLine
        putStrLn "Enter value for this key"  
        val <- getLine
        putStrLn "Enter path where to insert this key, enter :q to stop"  
        path <- getPath
        runStateT (runEffect $ parsingProducer hIn  >-> valueModifier (Add key val path)>-> bsWriter hOut) ([], Nothing)
      "REMOVE" -> do 
        putStrLn "Enter key to be removed"  
        key <- getLine
        putStrLn "Enter path where to insert this key, enter :q to stop"  
        path <- getPath
        x <- runStateT (runEffect $ parsingProducer hIn  >-> valueModifier (Remove key path) >-> waiter >-> bsWriter hOut) ([], Nothing)
        hPutStr hOut "}"
        return x
      _ -> putStrLn "Incorrect option" >> runStateT (runEffect $ parsingProducer hIn  >-> bsWriter hOut) ([], Nothing)

