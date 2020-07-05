{-# LANGUAGE OverloadedStrings, DeriveGeneric, NoMonomorphismRestriction, RankNTypes, BangPatterns #-}


module MyJSONData where

import qualified Data.HashMap.Strict as HM
import  Data.Text 

import Control.Lens (view,pre,ix)        -- from lens
import Control.Lens.Reified (ReifiedTraversal(..))
import Data.Aeson.Lens (_Object)  -- from lens-aeson

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

-- Object is one of several constructors of the Value type
-- Haskell Constructor | JSON Syntax
-- Object              | {"key": "value"}
-- String              | "hello"
-- Number              | 123
-- Array               | [1, 2, 3]

type MyJSONData = Value

showExtractProp :: String -> MyJSONData -> IO ()
showExtractProp t v = print $ extractProperty [pack t] v

showModifyObject :: [String] -> Object -> IO ()
showModifyObject t v = print $ modifyObjectNested (Prelude.map pack t) (Just . HM.insert "features" "MODIFIED") v

extractProperty :: [Text] -> MyJSONData -> Maybe MyJSONData
extractProperty []     v          = Just v
extractProperty (k:ks) (Object o) = HM.lookup k o >>= extractProperty ks
extractProperty _      _          = Nothing

modifyObject :: Text -> (Object -> Maybe Object) -> Object -> Maybe Object
modifyObject k f o = do
  subsetObjectX <- (HM.lookup k o) >>= getObject >>= f
  return $ HM.insert k (Object subsetObjectX) o

modifyObjectNested :: [Text] -> (Object -> Maybe Object) -> Object -> Maybe Object
modifyObjectNested (x:[]) f = modifyObject x f
modifyObjectNested (x:xs) f = modifyObject x (modifyObjectNested xs f)
modifyObjectNested [] _ = Just


--HELPERS

getObject :: MyJSONData -> Maybe Object
getObject (Object x) = Just x
getObject _ = Nothing

getText :: MyJSONData -> Maybe Text
getText (String x) = Just x
getText _ = Nothing
