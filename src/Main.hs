module Main where

import System.Environment
import Lib
import Control.Monad (unless)
import Pipes
import System.IO (isEOF)


main :: IO ()
main = do 
    {
    getArgs >>= mapM_ (\file -> readObjectFromFile file);
    putStrLn ("Press any key to exit");
    line <- getLine;
    putStrLn ("Exiting");

}