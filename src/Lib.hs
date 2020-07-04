{-# LANGUAGE OverloadedStrings, DeriveGeneric, NoMonomorphismRestriction, RankNTypes, BangPatterns #-}

module Lib where

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

bsBlocks path = do
  h <- lift $ IO.openFile path IO.ReadMode
  bsFromHandle (\h -> BS.hGet h 16384) h

-- read a line-oriented JSON file into object, counting parsing errors

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
                  lift $ putStrLn $ "got a Person: " ++ show a
                  doit (good+1) bad;
  (good,bad) <- evalStateT (doit 0 0) (bsBlocks path)
  print (good, bad)

