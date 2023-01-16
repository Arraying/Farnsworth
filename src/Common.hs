module Common
    (mapMany
    ) where

import           Errors


mapMany :: (a -> Either FWError b) -> [a] -> Either FWError [b]
mapMany _ [] = Right []
mapMany f (x:xs) = case f x of
  Left x' -> Left x'
  Right x' -> case mapMany f xs of
    Left y'  -> Left y'
    Right y' -> Right $ x' : y'


