
module HPage.Model (
    Page, Expression, fromString
 ) where

newtype Expression = Exp String
    deriving (Eq, Show)
 
data Page = Page { expressions :: [Expression] }

instance Show Page where
    show = show . expressions

fromString :: String -> Page
fromString = Page . (filter (/= Exp "")) . map (Exp . concat) . (splitOn "") . lines

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = reverse . (map reverse) . (foldl (\(acc:accs) el ->
                                                if el == sep
                                                then []:acc:accs
                                                else (el:acc):accs)
                                         [[]])