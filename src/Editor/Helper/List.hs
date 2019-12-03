module Editor.Helper.List(
  mkpairs
, removeDuplicates
)
where

import Data.List

-- create a list of pairs
mkpairs :: [a] -> [b] -> [(a,b)]
mkpairs xs ys = do x <- xs
                   y <- ys
                   return (x,y)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates l = foldr (\x xs -> if x `elem` xs then xs else x:xs) [] l
