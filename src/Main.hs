module Main where

import System.Environment
import Lib
import Control.Monad (unless)
import qualified Pipes.ByteString as P

import Pipes
import System.IO


main = withFile "testing.json"  ReadMode  $ \hIn  ->
       withFile "outFile.txt" WriteMode $ \hOut ->
       runEffect $ stdinProducer hIn >-> valueParser >-> bsWriter

-- main =do
--   runEffect $ stdinProducer2 >-> jsonArrayGrouper >-> randomSampler >-> bsWriter 

  -- getArgs >>= mapM_ (\file -> readObjectFromFileIntoJ file);
  --   --test2;
  --   putStrLn ("Press any key to exit");
  --   line <- getLine;
  --   putStrLn ("Exiting");