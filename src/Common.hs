module Common
    ( mapBin
    , mapRight
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
