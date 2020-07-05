{-# LANGUAGE OverloadedStrings, DeriveGeneric, NoMonomorphismRestriction, RankNTypes, BangPatterns #-}

module Lib where

import MyJSONData
import GHC.Generics

import Data.Text
import Data.Aeson
import Pipes
import Pipes.Parse
import qualified Pipes.Aeson as PA
import qualified Pipes.Prelude as P
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Control.Monad
import qualified Data.HashMap.Strict as HM

import Data.JsonStream.Parser
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson.Types as AT

import System.Random

import qualified System.IO as IO

test2 =  do
  Just ma <- evalStateT PA.decode (do mapM_ yield [" { \"age\":   234, \"name\": \"",  "Steve\" } "] )
  let _ = ma :: Either PA.DecodingError MyJSONData
  case ma of
    Left e   -> error $ "parse error: " ++ show e
    Right a  -> putStrLn $ "got a Person: " ++ show a


--The definition of Object in Data.Aeson is HashMap Text Value - 
--(you can think of them as glorified list of tuples; they even have list functions [2]).
-- read a line-oriented JSON file into object, counting parsing errors

valueParser :: Pipe B.ByteString (Value) IO ()
valueParser = do_parse parse_output
  where parse_output :: (ParseOutput Value)
        parse_output = runParser (value)
        do_parse :: ParseOutput Value -> Pipe B.ByteString (Value) IO ()
        do_parse output = case output of
            ParseYield value new_output -> do
              lift $ putStrLn "emitting new json value"
              lift $ print value
              yield value
              do_parse new_output
            ParseNeedData cont -> do
              lift $ putStrLn "in need of new data"
              input <- await
              lift $ putStrLn $ "injecting " ++ (B.unpack input) ++ " into the parser"
              do_parse $ cont input
            ParseDone remaining -> do
              lift $ putStrLn "parsing done"
              return ()
            ParseFailed err -> lift $ putStrLn $ "PARSING ERROR: " ++ err

stdinProducer :: IO.Handle -> Producer B.ByteString IO ()
stdinProducer h = do
  eof <- lift $ IO.hIsEOF h
  unless eof $ do
    str <- lift (B.hGet h 100)
    yield str
    stdinProducer h

bsWriter :: Consumer Value IO ()
bsWriter = do
  lift $ B.putStrLn "Writer"
  val <- await
  lift $ print val
  bsWriter

