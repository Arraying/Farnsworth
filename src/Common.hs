module Common
    ( binOps
    , mapBin
    , mapMany
    , mapRight
    , reserved
    , unOps
    ) where

import           Errors

mapRight :: (a -> Either FWError b) -> Either FWError a -> Either FWError b
mapRight f (Right x) = f x
mapRight _ (Left x)  = Left x

mapBin :: (a -> Either FWError b) -> (a, a) -> Either FWError (b, b)
mapBin f (l, r) = case l' of
  Left x -> Left x
  Right lv -> case r' of
    Left x   -> Left x
    Right rv -> Right (lv, rv)
  where
    l' = f l
    r' = f r

mapMany :: (a -> Either FWError b) -> [a] -> Either FWError [b]
mapMany _ [] = Right []
mapMany f (x:xs) = case f x of
  Left x' -> Left x'
  Right x' -> case mapMany f xs of
    Left y'  -> Left y'
    Right y' -> Right $ x' : y'

reserved :: [String]
reserved = unOps ++ binOps ++ ["if", "list", "anon"]

unOps :: [String]
unOps = ["-", "!", "head", "tail", "nil?", "list?"]

binOps :: [String]
binOps = ["*", "/", "&&", "||", "==", "!=", "<", ">", "<=", ">=", "cons"]
