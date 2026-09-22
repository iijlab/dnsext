{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DNS.Parser.State where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))

import DNS.Parser.Class

type Error = Last String
type Parser s = StateT s (StateT (Int, Int) (Except Error))

runError :: Error -> String
runError = fromMaybe "<empty error>" . getLast

runParser :: Parser s a -> s -> Either String (a, s)
runParser = runParserAt (1, 0)

-- | The same, told which line and column the input starts at.  What is
--   parsed a line at a time -- a zone file is, by its lexer -- would
--   otherwise report every error against line one.
runParserAt :: (Int, Int) -> Parser s a -> s -> Either String (a, s)
runParserAt pos p in_ = either (Left . runError) Right $ runExcept (evalStateT (runStateT p in_) pos)

instance CaseCons t s => MonadParser t s (Parser s) where
    getInput = get
    putInput = put
    raiseParser = lift . lift . throwE . Last . Just
    getPos = lift get
    putPos = lift . put
