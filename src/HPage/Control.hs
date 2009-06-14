{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

module HPage.Control (
    HPage, start, setText, evalFirst
 ) where

import Control.Monad.State
import Control.Monad.State.Class
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS

newtype Expression = Exp {asString :: String}
    deriving (Eq, Show)
 
data Page = Page { expressions :: [Expression],
                   server :: HS.ServerHandle }

instance Show Page where
    show = show . expressions

newtype HPageT m a = HPT { state :: StateT Page m a }
    deriving (Monad, MonadIO, MonadTrans)

type HPage = HPageT IO

start :: HPage ()
start = HPT $ do
               hs <- liftIO $ HS.start
               let emptyPage = Page [] hs
               put emptyPage

setText :: String -> HPage ()
setText s = HPT $ modify (\page -> page{expressions = fromString s})
    where fromString = (filter (/= Exp "")) . map (Exp . concat) . (splitOn "") . lines

evalFirst :: HPage (Either Hint.InterpreterError String)
evalFirst = HPT $ do
                    page <- get
                    let expr = asString $ head $ expressions page
                    let serv = server page
                    liftIO $ HS.runIn serv (Hint.eval expr)

-- PRIVATE FUNCTIONS -----------------------------------------------------------
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = reverse . (map reverse) . (foldl (\(acc:accs) el ->
                                                if el == sep
                                                then []:acc:accs
                                                else (el:acc):accs)
                                         [[]])