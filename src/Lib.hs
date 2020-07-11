{-# LANGUAGE OverloadedStrings, DeriveGeneric, NoMonomorphismRestriction, RankNTypes, BangPatterns #-}
{-|
Module      : Lib
Description : All the necessary functions to parse JSON token by token using Pipes.
Copyright   : Aleksandra Bulka
-}

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

-- |`TokenData` - a data type representing parts of the JSON file. It can be either: JString - a string, JNumber - a number, JOpenBracket - opening bracket ("{"),
-- |JCloseBracket - closing bracket ("}"), JComma - a comma (","), JTrue - boolean true, JFalse - boolean false, JNull - null value, JOpenSqBracket - opening square bracket ("["), 
-- |JCloseSqBracket - closing square bracket ("]"), JColon - a colon (":")
data TokenData = JString String | JNumber Double | JOpenBracket | JCloseBracket | JComma | JTrue | JFalse | JNull | JOpenSqBracket | JCloseSqBracket | JColon

-- |instance Show for TokenData, detemining how these parts of JSON file would be presented when printed
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

-- |function `write` for TokenData, detemining how these parts of JSON file would be written into a file
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

-- |`openBracket` - a parser that parses a single opening bracket ("{") character from Text to TokenData
openBracket :: AP.Parser TokenData
openBracket = const JOpenBracket <$> (AP.satisfy $ AP.inClass "{")

-- |`closeBracket` - a parser that parses a single closing bracket ("}") character from Text to TokenData
closeBracket :: AP.Parser TokenData
closeBracket = const JCloseBracket <$> (AP.satisfy $ AP.inClass "}")

-- |`comma` - a parser that parses a single comma (",") character from Text to TokenData
comma :: AP.Parser TokenData
comma = const JComma <$> (AP.satisfy $ AP.inClass ",")

-- |`colon` - a parser that parses a single colon (":") character from Text to TokenData
colon :: AP.Parser TokenData
colon = const JColon <$> (AP.satisfy $ AP.inClass ":")

-- |`openSqBracket` - a parser that parses a single opening square bracket ("[") character from Text to TokenData
openSqBracket :: AP.Parser TokenData
openSqBracket = const JOpenSqBracket <$> (AP.satisfy $ AP.inClass "[")

-- |`closeSqBracket` - a parser that parses a single closing square bracket ("]") character from Text to TokenData
closeSqBracket :: AP.Parser TokenData
closeSqBracket = const JCloseSqBracket <$> (AP.satisfy $ AP.inClass "]")

-- |`nullParser` - a parser that parses a string containing null value ("null") from Text to TokenData
nullParser :: AP.Parser TokenData
nullParser = AP.string "null" *> return JNull

-- |`boolParser` - a parser that parses a string containing boolean value (either "true" or "false") from Text to TokenData
boolParser :: AP.Parser TokenData
boolParser = (trueParser *> return JTrue) <|> (falseParser *> return JFalse)
  where
    trueParser = AP.string "true" 
    falseParser = AP.string "false" 

-- |`doubleParser` - a parser that parses a string containing a number from Text to TokenData
doubleParser ::  AP.Parser TokenData
doubleParser = fmap JNumber AP.double

-- |`stringParser` - a parser that parses a string containing some text from Text to TokenData
stringParser ::  AP.Parser TokenData
stringParser = do
    AP.string "\""
    s <- AP.takeWhile (/= '\"' )
    AP.string "\""
    return $ JString (unpack s)

-- |`parseJsonData` - a parser that chooses the parser from the ones defined above to parse any Text to TokenData.
-- |It skips over whitespace characters in text.
parseJsonData :: AP.Parser TokenData
parseJsonData = AP.skipSpace *> AP.choice [openBracket, closeBracket, comma, colon, openSqBracket, closeSqBracket, boolParser, doubleParser, stringParser, nullParser]

-- |`JStack` - a new data type representing the position in the currently parsed JSON file. It is a stack. A current top element on the stacks represents the part of the
-- |JSON file we are currently inside. JArray means an arrar and JObject means an object (key-value pair). JObject parametrized by Nothing means we are indside the object but have not 
-- | parsed its key yet. JObject parametrized by some string means we have already parsed the objects' key.
data JStack = JArray | JObject (Maybe String) 

-- |`prefix` - a helper function to assert whether the first list is a prefix of a second list. In other words - are all of the elements of the first list 
-- |the corresponding first elements of the second list.
prefix :: [String] -> [String] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

-- |`calcPath` - a helper function parsing the current positon in the JSON file in the form of a stack of JStacks into a more readable list of the nodes - 
-- |from the biggest node we are inside of to the most direct parent of a node we are inside of.
calcPath :: [JStack] -> [String]
calcPath = Prelude.reverse . mapMaybe fun  
  where 
      fun JArray = Nothing
      fun (JObject s) = s

-- |`valueChanger` function to update a node. It updates an object with key `k` with a value `v`.  It returns a new value for the key to be yielded.
valueChanger :: String -> String -> TokenData -> [JStack] -> Bool -> TokenData
valueChanger k v curr_val (x:xs) is_key = case x of
                                               (JObject (Just key)) -> do
                                                    case curr_val of 
                                                        (JString s) -> if ((not is_key) && key == k) then (JString v) else curr_val
                                                        JTrue -> if key == k then (JString v) else curr_val
                                                        JFalse -> if key == k then (JString v) else curr_val
                                                        JNull -> if key == k then (JString v) else curr_val
                                                        (JNumber n) -> if key == k then (JString v) else curr_val
                                                        _ ->  curr_val
                                               _ ->  curr_val

-- |`valueAdderToVal` checks if the key where the new object is to be inserted contains only a single value (string/number/bool/null). 
-- |It finds the key where we wish to insert by the `desired_path` parameter.
valueAdderToVal :: [String] -> TokenData -> [JStack] -> Bool -> Bool
valueAdderToVal desired_path curr_val curr_stack is_key = case curr_val of
                                                (JString s) -> if (calcPath curr_stack) == desired_path  && (not is_key) then True else False
                                                JTrue -> if (calcPath curr_stack) == desired_path then True else False
                                                JFalse -> if (calcPath curr_stack) == desired_path then True else False
                                                JNull -> if (calcPath curr_stack) == desired_path then True else False
                                                (JNumber n) ->if (calcPath curr_stack) == desired_path then True else False
                                                _ -> False

-- |`valueAdderToObj` checks if the key where the new object is to be inserted contains only a nested object.
-- |It finds the key where we wish to insert by the `desired_path` parameter.
valueAdderToObj :: [String] -> TokenData -> [JStack] -> Bool -> Bool
valueAdderToObj desired_path curr_val curr_stack is_key = case curr_val of
                                                JOpenBracket -> if (calcPath curr_stack) == desired_path then True else False
                                                _ -> False

-- |`valueAdder` function to add a node. It adds an object with key `k` with a value `v`. Either `valueAdderToVal` is chosen if we would like to add to and object containing only string/number/bool/null as value
-- | or `valueAdderToObj` is chosen if we would like to add to and object containing a nested object as value. List of values to be yielded is then returned.
valueAdder :: String -> String -> [String] -> TokenData -> [JStack] -> Bool -> [TokenData]
valueAdder k v desired_path curr_val curr_stack is_key = if (valueAdderToVal desired_path curr_val curr_stack is_key) then [JOpenBracket, (JString k), JColon, (JString v), JCloseBracket] else 
                                                         if (valueAdderToObj desired_path curr_val curr_stack is_key) then [JOpenBracket, (JString k), JColon, (JString v), JComma] else [curr_val]


-- |`valueRemover` function to remove the value under the key `k`. It finds the key where we wish to remove by the `desired_path` parameter.
valueRemover :: String -> [String] -> TokenData -> [JStack] -> Bool -> [TokenData]
valueRemover key desired_path curr_val curr_stack is_key = if (desired_path++[key]) `prefix` (calcPath curr_stack) then [] else [curr_val]

-- |Helper function to modify first element of the tuple
modFst :: (a -> a) -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
modFst f = modify (\(a,b) -> (f a, b))

-- |Helper function to modify second element of the tuple
modSnd :: (b -> b) -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
modSnd f =  modify (\(a,b) -> (a,f b))

-- |Helper function to put a value as a first element of the tuple
putFst :: a -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
putFst x = modify  (\(a,b) -> (x, b))

-- |Helper function to put a value as a second element of the tuple
putSnd ::  b -> Pipe TokenData (TokenData) (StateT (a,b) IO) ()
putSnd x = modify  (\(a,b) -> (a, x))

-- |A type describing the operation that is to be performed on a JSON file. Either Modify (parametrized by a key - to be modified and a value - to replace the old value), Add (parametrized by the key of the 
-- |object to add, value of the object to add and a humanly readable path - as in`calcPath` to where to add the new object) or Remove (parametrized by a key of the object to be removed and a path (like in `Add`)).
data OperationType = Modify String String | Add String String [String] | Remove String [String] 


-- |A pipe accepting OperationType (an operation to be performed on a JSON) as a parameter and outputting a TokenData of each transformed part of JSON.
-- |Uses a tuple of list of JStack and TokenData to keep state and effectively using only the first element of the tuple to store the current position in the JSON document.
valueModifier :: OperationType -> Pipe TokenData (TokenData) (StateT ([JStack],Maybe TokenData) IO) a
valueModifier operation = do 
    val <- await
    -- * Create a [JStack] representation of where in the JSON file the currently processed value is, and also checks if currently processed value is a key.
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
        --  Perform a proper operation on the currently processed value.
            case operation of 
                (Modify k v) -> yield $ valueChanger k v val curr_stack isKey
                (Add k v path) -> mapM_ yield (valueAdder k v path val curr_stack isKey)
                (Remove k path) -> mapM_ yield (valueRemover k path val curr_stack isKey)
        _ -> yield val
    valueModifier operation   

-- |A producer reading a file handle (as Text) and yielding it chunk by chunk.
stdinProducer :: (MonadIO m) => IO.Handle -> Producer Text m ()
stdinProducer h = do
  eof <- liftIO $ IO.hIsEOF h
  unless eof $ do
    str <- liftIO (TIO.hGetChunk h)
    yield str
    stdinProducer h

-- |A producer using a `stdinProducer` to read data but additionaly parses them using the `parseJsonData` parser into TokenData (parts of JSON file).
parsingProducer ::(MonadIO m) =>  IO.Handle -> Producer TokenData m (Either (PAP.ParsingError, Producer Text m ()) ())
parsingProducer h = (PAP.parsed parseJsonData (stdinProducer h))
 
-- |A pipe only used when removing a node, removes additional commas left after removal of node.
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

-- |A consumer receiving TokenData and writing it to an output file.
bsWriter :: (MonadIO m) => IO.Handle  -> Consumer (TokenData) m a
bsWriter h = do
  val <- await
  liftIO $ IO.hPutStr h $ write val
  bsWriter h

