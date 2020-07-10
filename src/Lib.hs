{-# LANGUAGE OverloadedStrings, DeriveGeneric, NoMonomorphismRestriction, RankNTypes, BangPatterns #-}

module Lib where

import Data.Maybe
import Data.Text
import Data.String

import Pipes
import Pipes.Parse
import Pipes.Lift

import Control.Monad
import Control.Monad.State.Strict
import Control.Applicative

import qualified System.IO as IO
import qualified Data.Text.IO as TIO

import qualified Pipes.Attoparsec as PAP
import qualified Data.Attoparsec.Text as AP

data OperationType = Modify String String | Add String String [String] | Remove String [String] 

data TokenData = JString String | JNumber Double | JOpenBracket  | JCloseBracket  | JComma | JTrue | JFalse | JNull | JOpenSqBracket | JCloseSqBracket | JColon

instance Show TokenData where
  show (JString s) = show s
  show (JNumber num) = show num
  show JOpenBracket = show '{'
  show JCloseBracket = show "}"
  show JComma = show ","
  show JColon = show ":"
  show JOpenSqBracket = show "["
  show JCloseSqBracket = show "]"
  show JTrue = show "true"
  show JFalse = show "false"
  show JNull = show "null"

write :: TokenData -> [Char]
write (JString s) = show s
write (JNumber num) = show num
write JOpenBracket = ['{']
write JCloseBracket = ['}']
write JComma = [',']
write JColon = [':']
write JOpenSqBracket = ['[']
write JCloseSqBracket = [']']
write JTrue = "true"
write JFalse = "false"
write JNull = "null"


openBracket :: AP.Parser TokenData
openBracket = const JOpenBracket <$> (AP.satisfy $ AP.inClass "{")

closeBracket :: AP.Parser TokenData
closeBracket = const JCloseBracket <$> (AP.satisfy $ AP.inClass "}")

comma :: AP.Parser TokenData
comma = const JComma <$> (AP.satisfy $ AP.inClass ",")

colon :: AP.Parser TokenData
colon = const JColon <$> (AP.satisfy $ AP.inClass ":")

openSqBracket :: AP.Parser TokenData
openSqBracket = const JOpenSqBracket <$> (AP.satisfy $ AP.inClass "[")

closeSqBracket :: AP.Parser TokenData
closeSqBracket = const JCloseSqBracket <$> (AP.satisfy $ AP.inClass "]")

nullParser :: AP.Parser TokenData
nullParser = AP.string "null" *> return JNull

boolParser :: AP.Parser TokenData
boolParser = (trueParser *> return JTrue) <|> (falseParser *> return JFalse)
  where
    trueParser = AP.string "true" 
    falseParser = AP.string "false" 

doubleParser ::  AP.Parser TokenData
doubleParser = fmap JNumber AP.double

stringParser ::  AP.Parser TokenData
stringParser = do
    AP.string "\""
    s <- AP.takeWhile (/= '\"' )
    AP.string "\""
    return $ JString (unpack s)

parseJsonData :: AP.Parser TokenData
parseJsonData = AP.skipSpace *> AP.choice [openBracket, closeBracket, comma, colon, openSqBracket, closeSqBracket, boolParser, doubleParser, stringParser, nullParser ]


data JStack = JArray | JObject (Maybe String) 
 

prefix :: [String] -> [String] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

calcPath :: [JStack] -> [String]
calcPath = Prelude.reverse . mapMaybe fun  
  where 
      fun JArray = Nothing
      fun (JObject s) = s

valueChanger :: String -> String -> TokenData -> [JStack] -> Bool -> TokenData
valueChanger k v curr_val (x:xs) is_key = case x of
                                               (JObject (Just key)) -> do
                                                    case curr_val of 
                                                        (JString s) -> if ((not is_key) && key == k ) then (JString v) else curr_val
                                                        JTrue -> if key == k then (JString v) else curr_val
                                                        JFalse -> if key == k then (JString v) else curr_val
                                                        JNull -> if key == k then (JString v) else curr_val
                                                        (JNumber n) -> if key == k then (JString v) else curr_val
                                                        _ ->  curr_val
                                               _ ->  curr_val


valueAdderToVal :: [String] -> TokenData -> [JStack] -> Bool -> Bool
valueAdderToVal desired_path curr_val curr_stack is_key = case curr_val of
                                                (JString s) -> if (calcPath curr_stack) == desired_path  && (not is_key) then True else False
                                                JTrue -> if (calcPath curr_stack) == desired_path then True else False
                                                JFalse -> if (calcPath curr_stack) == desired_path then True else False
                                                JNull -> if (calcPath curr_stack) == desired_path then True else False
                                                (JNumber n) ->if (calcPath curr_stack) == desired_path then True else False
                                                _ -> False

valueAdderToObj :: [String] -> TokenData -> [JStack] -> Bool -> Bool
valueAdderToObj desired_path curr_val curr_stack is_key = case curr_val of
                                                JOpenBracket -> if (calcPath curr_stack) == desired_path then True else False
                                                _ -> False


valueAdder :: String -> String -> [String] -> TokenData -> [JStack] -> Bool -> [TokenData]
valueAdder k v desired_path curr_val curr_stack is_key = if (valueAdderToVal desired_path curr_val curr_stack is_key) then [JOpenBracket, (JString k), JColon, (JString v), JCloseBracket] else 
                                                         if (valueAdderToObj desired_path curr_val curr_stack is_key) then [JOpenBracket, (JString k), JColon, (JString v), JComma] else [curr_val]


valueRemover :: String -> [String] -> TokenData -> [JStack] -> Bool -> [TokenData]
valueRemover key desired_path curr_val curr_stack is_key = if (desired_path++[key]) `prefix` (calcPath curr_stack) then [] else [curr_val]

modFst :: (a -> a) -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
modFst f = modify (\(a,b) -> (f a, b))

modSnd :: (b -> b) -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
modSnd f =  modify (\(a,b) -> (a,f b))

putFst :: a -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
putFst x = modify  (\(a,b) -> (x, b))

putSnd ::  b -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
putSnd x = modify  (\(a,b) -> (a, x))




valueModifier :: OperationType -> Pipe TokenData (TokenData) (StateT ([JStack],Maybe TokenData) IO) a
valueModifier operation = do 
    val <- await
    isKey <- case val of
                JOpenBracket -> modFst (JObject Nothing :) >> return False
                (JString s) -> do
                    (x:xs) <- fst <$> get 
                    case x of 
                        (JObject Nothing) -> putFst (JObject (Just s):xs)  >> return True
                        _ -> return False
                JComma -> do
                    (x:xs) <- fst <$> get
                    case x of 
                        (JObject s) -> do putFst (JObject Nothing:xs)  >> return False
                        _ -> return False
                JCloseBracket -> modFst Prelude.tail  >> return False
                JOpenSqBracket -> modFst (JArray :)  >> return False
                JCloseSqBracket ->  modFst Prelude.tail  >> return False
                _ -> return False
    curr_stack <- fst <$> get 
    case curr_stack of
        (x:xs) -> do
            case operation of 
                (Modify k v) -> yield $ valueChanger k v val curr_stack isKey
                (Add k v path) -> mapM_ yield (valueAdder k v path val curr_stack isKey)
                (Remove k path) -> mapM_ yield (valueRemover k path val curr_stack isKey)
        _ -> yield val
  
    valueModifier operation
    

stdinProducer :: (MonadIO m) => IO.Handle -> Producer Text m ()
stdinProducer h = do
  eof <- liftIO $ IO.hIsEOF h
  unless eof $ do
    str <- liftIO (TIO.hGetChunk h)
    yield str
    stdinProducer h

parsingProducer ::(MonadIO m) =>  IO.Handle -> Producer TokenData m (Either (PAP.ParsingError, Producer Text m ()) ())
parsingProducer h = PAP.parsed parseJsonData (stdinProducer h)

waiter :: Pipe TokenData (TokenData) (StateT ([JStack], Maybe TokenData) IO)  a
waiter = do
    val_new <- await
    val_old <- snd <$> get 
    case val_old of
        Nothing -> do
            putSnd (Just val_new)
        Just JComma -> case val_new of
            JComma ->  putSnd (Just val_new)
            JCloseBracket -> putSnd (Just val_new)
            x -> yield JComma >> putSnd (Just val_new)
        Just JOpenBracket -> case val_new of
            JComma ->  putSnd (Just JOpenBracket)
            x -> yield JOpenBracket >> putSnd (Just val_new)
        Just x -> yield x >> putSnd (Just val_new)
    waiter


bsWriter :: (MonadIO m) => IO.Handle  -> Consumer (TokenData) m a
bsWriter h = do
  val <- await
  liftIO $ IO.hPutStr h $ write val
  bsWriter h

