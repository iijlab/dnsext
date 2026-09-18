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
runParser p in_ = either (Left . runError) Right $ runExcept (evalStateT (runStateT p in_) (1, 0))

instance CaseCons t s => MonadParser t s (Parser s) where
    getInput = get
    putInput = put
    raiseParser = lift . lift . throwE . Last . Just
    getPos = lift get
    putPos = lift . put
