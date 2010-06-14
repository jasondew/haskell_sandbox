module JSON
       ( JValue(..)
       , getString
       , getInt
       , getDouble
       , getBool
       , getObject
       , getArray
       , isNull
       ) where

import Control.Monad (forM_)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _ = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

putJValue :: JValue -> IO ()
putJValue (JString s) = putStr (show s)
putJValue (JNumber n) = putStr (show n)
putJValue (JBool True) = putStr "true"
putJValue (JBool False) = putStr "false"
putJValue JNull = putStr "null"

putJValue (JObject xs) = do
  putChar '{'
  putPairArray xs
  putChar '}'
  where putPair (k,v) = do putStr (show k)
                           putStr ": "
                           putJValue v
        putPairArray []     = putStr ""
        putPairArray (p:ps) = do
                                putPair p
                                forM_ ps $ \q -> do putStr ", "; putPair q
