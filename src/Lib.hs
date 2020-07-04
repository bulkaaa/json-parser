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

import qualified System.IO as IO

test2 =  do
  Just ma <- evalStateT PA.decode (do mapM_ yield [" { \"age\":   234, \"name\": \"",  "Steve\" } "] )
  let _ = ma :: Either PA.DecodingError MyJSONData
  case ma of
    Left e   -> error $ "parse error: " ++ show e
    Right a  -> putStrLn $ "got a Person: " ++ show a


-- example of how to use `encodeArray`
--test = runEffect $ PA.encodeArray peopleVector >-> (forever $ await >>= (lift . BS.putStr) )

bsFromHandle :: MonadIO m => (IO.Handle -> IO BS.ByteString) -> IO.Handle -> Producer' BS.ByteString m ()
bsFromHandle reader h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            str <- liftIO $ reader h
            yield str
            go
{-# INLINABLE bsFromHandle #-}

--TODO: find out difference
bsLines :: [Char] -> Producer' BS.ByteString IO ()
bsLines path = do
  h <- lift $ IO.openFile path IO.ReadMode
  bsFromHandle BS.hGetLine h

bsBlocks :: [Char] -> Producer' BS.ByteString IO ()
bsBlocks path = do
  h <- lift $ IO.openFile path IO.ReadMode
  bsFromHandle (\h -> BS.hGet h 16384) h


--The definition of Object in Data.Aeson is HashMap Text Value - 
--(you can think of them as glorified list of tuples; they even have list functions [2]).
-- read a line-oriented JSON file into object, counting parsing errors

readObjectFromFile :: [Char] -> IO ()
readObjectFromFile path = do
  let doit !good !bad = do
        m <- PA.decode
        let _ = m :: Maybe (Either PA.DecodingError Value)
        case m of
          Nothing        -> return (good,bad)
          Just x         -> do
            case x of
              Left e  -> do 
                  lift $ putStrLn "parse error"; 
                  doit good (bad+1);
              Right a -> do 
                  lift $ putStrLn "ok"; 
                  lift $ putStrLn $ "got aN OBJecT: " ++ show a
                  doit (good+1) bad;
  (good,bad) <- evalStateT (doit 0 0) (bsLines path)
  print (good, bad)

readObjectFromFileIntoJ :: [Char] -> IO ()
readObjectFromFileIntoJ path = do
  let doit !good !bad = do
        m <- PA.decode
        let _ = m :: Maybe (Either PA.DecodingError Value)
        case m of
          Nothing        -> return (good,bad)
          Just x         -> do
            case x of
              Left e  -> do 
                  lift $ putStrLn "parse error"; 
                  doit good (bad+1)
              Right a -> do 
                  lift $ putStrLn "OK";
                  lift $ print a;
                  v <- lift $ showExtractProp ("bigNode3"::String) a;
                  doit (good+1) bad
  (good,bad) <- evalStateT (doit 0 0) (bsLines path)
  print (good, bad)
