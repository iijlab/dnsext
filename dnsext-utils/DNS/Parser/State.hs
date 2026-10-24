{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DNS.Parser.State where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)

{- The strict state, not the lazy one.  What a parser keeps in its state
   is the input it has still to read, and the lazy StateT leaves a thunk
   for it at every step -- a chain as long as the input, held until
   something forces it.  Reading a zone of sixty thousand records holds
   557 MB of heap that way, and 193 MB this way. -}
import Control.Monad.Trans.State.Strict
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
    {-# INLINE getInput #-}
    putInput = put
    {-# INLINE putInput #-}
    raiseParser = lift . lift . throwE . Last . Just
    {-# INLINE raiseParser #-}
    getPos = lift get
    {-# INLINE getPos #-}
    putPos = lift . put
    {-# INLINE putPos #-}
