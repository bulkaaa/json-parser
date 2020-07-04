{-# LANGUAGE OverloadedStrings, DeriveGeneric, NoMonomorphismRestriction, RankNTypes, BangPatterns #-}


module MyJSONData where
import Pipes
import Pipes.Parse
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import GHC.Generics
import Data.Aeson

-- Object is one of several constructors of the Value type

-- Haskell Constructor | JSON Syntax
-- Object              | {"key": "value"}
-- String              | "hello"
-- Number              | 123
-- Array               | [1, 2, 3]

data MyJSONData = MyJSONData Value deriving (Show, Generic)
-- NOTE: I am using `Object` and not `Value`. Because I do not 
--want it to be any JSON value, but specifically objects.

instance ToJSON MyJSONData 
instance FromJSON MyJSONData 


showExtractProp :: String -> Value -> IO ()
showExtractProp t v = print $ extractProperty [T.pack t] v

extractProperty :: [T.Text] -> Value -> Maybe Value
extractProperty []     v          = Just v
extractProperty (k:ks) (Object o) = HM.lookup k o >>= extractProperty ks
extractProperty _      _          = Nothing

-- data JValue = JString String
--             | JNumber Double
--             | JBool Bool
--             | JNull
--             | JObject [(String, JValue)]
--             | JArray [JValue]
--             | JChar [Char]
--               deriving (Eq, Ord, Show, Generic)


-- instance ToJSON JValue
-- instance FromJSON JValue


-- getString :: JValue -> Maybe String
-- getString (JString s) = Just s
-- getString _           = Nothing

-- getInt :: JValue -> Maybe Int
-- getInt (JNumber n) = Just (truncate n)
-- getInt _           = Nothing

-- getDouble :: JValue -> Maybe Double
-- getDouble (JNumber n) = Just n
-- getDouble _           = Nothing

-- getBool :: JValue -> Maybe Bool
-- getBool (JBool b) = Just b
-- getBool _         = Nothing

-- getObject :: JValue -> Maybe [(String, JValue)]
-- getObject (JObject o) = Just o
-- getObject _           = Nothing

-- getArray :: JValue -> Maybe [JValue]
-- getArray (JArray a) = Just a
-- getArray _          = Nothing

-- isNull :: JValue -> Bool
-- isNull v            = v == JNull


-- -- renderJValue :: JValue -> String

-- -- renderJValue (JString s)   = show s
-- -- renderJValue (JNumber n)   = show n
-- -- renderJValue (JBool True)  = "true"
-- -- renderJValue (JBool False) = "false"
-- -- renderJValue JNull         = "null"

-- -- renderJValue (JObject o) = "{" ++ pairs o ++ "}"
-- --   where pairs [] = ""
-- --         pairs ps = intercalate ", " (map renderPair ps)
-- --         renderPair (k,v)   = show k ++ ": " ++ renderJValue v

-- -- renderJValue (JArray a) = "[" ++ values a ++ "]"
-- --   where values [] = ""
-- --         values vs = intercalate ", " (map renderJValue vs)

-- -- putJValue :: JValue -> IO ()
-- -- putJValue v = putStrLn (renderJValue v)