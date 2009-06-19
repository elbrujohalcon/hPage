{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

module HPage.Control (
    HPage, setText, evalFirst, evalHPage
 ) where

import Control.Monad.State
import Control.Monad.State.Class
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS

newtype Expression = Exp {asString :: String}
    deriving (Eq)

instance Show Expression where
    show = asString
 
data Page = Page { expressions :: [Expression],
                   server :: HS.ServerHandle }

instance Show Page where
    show = show . expressions

newtype HPageT m a = HPT { state :: StateT Page m a }
    deriving (Monad, MonadIO, MonadTrans)

type HPage = HPageT IO

setText :: String -> HPage ()
setText s = HPT $ do
                    let x = fromString s
                    liftIO $ putStrLn $ show x
                    modify (\page -> page{expressions = fromString s})
    where fromString = filter (/= Exp "") . map toExp . splitOn "" . lines
          toExp = Exp . concat . map ('\n':)

evalFirst :: HPage (Either Hint.InterpreterError String)
evalFirst = HPT $ do
                    page <- get
                    let expr = asString $ head $ expressions page
                    let serv = server page
                    liftIO $ HS.runIn serv (Hint.eval expr)
                    
evalHPage :: HPage a -> IO a
evalHPage hpt = do
                    hs <- liftIO $ HS.start
                    let emptyPage = Page [] hs
                    (state hpt) `evalStateT` emptyPage 

-- PRIVATE FUNCTIONS -----------------------------------------------------------
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = reverse . (map reverse) . (foldl (\(acc:accs) el ->
                                                if el == sep
                                                then []:acc:accs
                                                else (el:acc):accs)
                                         [[]])