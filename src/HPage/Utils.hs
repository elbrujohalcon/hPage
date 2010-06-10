
module HPage.Utils where

import Data.List (isPrefixOf)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn [] xs = [xs]
splitOn _ [] = []
splitOn s xs = splitOn' [] s xs

splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
splitOn' [] _ [] = []
splitOn' acc _ [] = [reverse acc]
splitOn' acc s r@(x:xs)
    | isPrefixOf s r = (reverse acc) : splitOn' [] s (drop (length s) r)
    | otherwise = splitOn' (x:acc) s xs    

joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith sep (x:xs) = x ++ (concat . map (sep ++) $ xs)

insertAt :: Int -> [a] -> [a] -> [a]
insertAt 0 new [] = new
insertAt i new old | i == length old = old ++ new
                   | otherwise = let (before, (_:after)) = splitAt i old
                                  in before ++ new ++ after

showWithCurrent :: Show a => [a] -> Int -> String -> (String -> String) -> String
showWithCurrent allItems curItem sep mark = 
        drop (length sep) . concat $ map (showNth allItems curItem) [0..itemCount - 1]
    where itemCount  = length allItems
          showNth list sel cur = sep ++ ((if sel == cur then mark else id) $ show $ list !! cur)

