{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module DNS.Parser.Class where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr)
import Data.Functor
import Data.Word (Word8)

{- FOURMOLU_DISABLE -}
class ParserToken t where
    proceed :: t -> (Int, Int) -> (Int, Int)
    proceed _ (lin, col) = (lin, col + 1)
    {-# INLINEABLE proceed #-}

class ParserToken t => CaseCons t s | s -> t where
    caseCons :: (t -> s -> a) -> a -> s -> a

class (Monad m, Alternative m, CaseCons t s) => MonadParser t s m | m -> s where
    getInput     :: m s
    putInput     :: s -> m ()
    raiseParser  :: String -> m a
    getPos       :: m (Int, Int)
    getPos       =  pure (-1, -1)
    {-# INLINEABLE getPos #-}
    putPos       :: (Int, Int) -> m ()
    putPos _     =  pure ()
    {-# INLINEABLE putPos #-}
{- FOURMOLU_ENABLE -}

------------------------------------------------------------

{- FOURMOLU_DISABLE -}
instance ParserToken Word8 where
    proceed b = proceedChar (w8toChar b)

w8toChar :: Word8 -> Char
w8toChar = chr . fromIntegral
{-# INLINEABLE w8toChar #-}

instance ParserToken Char where
    proceed = proceedChar

proceedChar :: Char -> (Int, Int) -> (Int, Int)
proceedChar c (lin, col) = case c of
    '\n'  -> (lin + 1, 0)
    _     -> (lin, col + 1)

instance CaseCons Word8 BS.ByteString where
    caseCons c n bs
        | BS.null bs  = n
        | otherwise   = c (BS.head bs) (BS.tail bs)

instance CaseCons Word8 LB.ByteString where
    caseCons c n bs
        | LB.null bs  = n
        | otherwise   = c (LB.head bs) (LB.tail bs)

instance ParserToken a => CaseCons a [a] where
    caseCons c n xxs = case xxs of
        []      -> n
        x : xs  -> c x xs
{- FOURMOLU_ENABLE -}

------------------------------------------------------------

{- FOURMOLU_DISABLE -}
{-# INLINEABLE takeCons #-}
takeCons :: CaseCons t s => Int -> s -> [t]
takeCons n s
    | n <= 0     = []
    | otherwise  = caseCons (\t ts -> t : takeCons (n-1) ts) [] s

{-# INLINEABLE parseError #-}
parseError :: MonadParser t s m => String -> m a
parseError s = do
    (lin, col) <- getPos
    raiseParser $ showPos lin col ++ s
  where
    showPos lin col
        | lin < 0    = ""
        | otherwise  = "line " ++ show lin ++ ", column " ++ show col ++ ": "

{-# INLINEABLE token #-}
token :: MonadParser t s m => m t
token = caseCons cons nil =<< getInput
  where
    cons t ts = (putPos . proceed t =<< getPos) *> putInput ts $> t
    nil = parseError "token: eof"

{-# INLINEABLE eof #-}
eof :: (Show t, MonadParser t s m) => m ()
eof = do
    s <- getInput
    caseCons (cons s) nil s
  where
    cons s _ _ = parseError $ "eof: more inputs found: " ++ unwords (map show $ takeCons 7 s) ++ " ..."
    nil = pure ()

{-# INLINEABLE lookAhead #-}
lookAhead :: MonadParser t s m => m a ->  m a
lookAhead px = do
    s <- getInput
    p <- getPos
    x <- px
    putPos p
    putInput s
    pure x
{- FOURMOLU_ENABLE -}

------------------------------------------------------------

{- FOURMOLU_DISABLE -}
{-# INLINEABLE satisfy #-}
satisfy :: (Show t, MonadParser t s m) => String -> (t -> Bool) -> m t
satisfy name p = do
    t <- token
    guard (p t) <|> parseError ("satisfy: not satisfied, <" ++ name ++ "> predicate against " ++ show t)
    pure t

{-# INLINEABLE this #-}
this :: (Eq t, Show t, MonadParser t s m) => t -> m t
this tk = satisfy ("this " ++ show tk) (== tk)

{-# INLINEABLE these #-}
these :: (Eq t, Show t, MonadParser t s m) => [t] -> m [t]
these = mapM this

{-# INLINEABLE choice #-}
choice :: MonadParser t s m => [m a] -> m a
choice  []         = parseError "choice: no fallbacks"
choice [x]         = x
choice (x:xs@(_:_))  = x <|> choice xs

{-# INLINEABLE readable #-}
readable :: (Read a, MonadParser t s m) => String -> String -> m a
readable name str =
    case [ x | (x, "") <- reads str ] of
        []   -> parseError $ "readable: " ++ name ++ ": unable to read: " ++ str
        x:_  -> pure x
{- FOURMOLU_ENABLE -}
