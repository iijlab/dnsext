{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module DNS.Parser.Class where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr)
import Data.Functor
import Data.List (foldl')
import Data.Word (Word8)

{- FOURMOLU_DISABLE -}
class ParserToken t where
    proceed :: t -> (Int, Int) -> (Int, Int)
    proceed _ (lin, col) = (lin, col + 1)
    {-# INLINEABLE proceed #-}

class ParserToken t => CaseCons t s | s -> t where
    caseCons :: (t -> s -> a) -> a -> s -> a
    -- | The longest run of tokens the predicate takes, and what is left
    --   after it.  One at a time unless the input knows better, which a
    --   ByteString does.
    spanCons :: (t -> Bool) -> s -> ([t], s)
    spanCons p = loop
      where
        loop s = caseCons taken ([], s) s
          where
            taken t ts
                | p t = let (us, s') = loop ts in (t : us, s')
                | otherwise = ([], s)
    {-# INLINEABLE spanCons #-}

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
    spanCons p bs = case BS.span p bs of (run, rest) -> (BS.unpack run, rest)

instance CaseCons Word8 LB.ByteString where
    caseCons c n bs
        | LB.null bs  = n
        | otherwise   = c (LB.head bs) (LB.tail bs)
    spanCons p bs = case LB.span p bs of (run, rest) -> (LB.unpack run, rest)

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

-- | The token which comes next, left where it is.  'lookAhead' 'token'
--   does the same by taking it and putting the input and the position
--   back; this reads and writes nothing, which is what a parser wants
--   when it is deciding which way to go.
{-# INLINEABLE peek #-}
peek :: MonadParser t s m => m t
peek = caseCons cons nil =<< getInput
  where
    cons t _ = pure t
    nil = parseError "peek: eof"

-- | As many tokens in a row as the predicate takes, taken at once.
--
--   'satisfy' reads the input, reads and writes the position and writes
--   the input back for every token; a run of a hundred bytes is a
--   hundred of each.  The input finds the end of the run by itself --
--   for a ByteString that is a scan and no allocation -- and the
--   position moves once for the lot.
{-# INLINEABLE spanning #-}
spanning :: MonadParser t s m => (t -> Bool) -> m [t]
spanning p = do
    s <- getInput
    case spanCons p s of
        (ts, s') -> do
            pos <- getPos
            putPos $ foldl' (flip proceed) pos ts
            putInput s'
            pure ts

-- | A parser run for as long as it goes on succeeding, and what it
--   gave each time.
--
--   'Control.Applicative.many' is this, and cannot be used where the
--   input is long.  It is @some p \<|\> pure []@ with @some p = (:)
--   \<$\> p \<*\> many p@, so the alternatives nest one inside another,
--   one per turn, and each of them is holding the input it would go
--   back to if what came after it failed.  None of that is let go until
--   the whole thing ends, so reading a file keeps every token of the
--   file, and the stack to match.  A zone of a hundred and eighty
--   thousand records held 307 MB that way and 117 MB this way.
--
--   Here the alternative covers one turn, so what it holds is dropped
--   as soon as that turn succeeds, and the next turn is in the
--   continuation of a bind rather than inside the alternative.
{-# INLINEABLE repeatedly #-}
repeatedly :: MonadParser t s m => m a -> m [a]
repeatedly p = go id
  where
    go acc = do
        m <- optional p
        case m of
            Nothing -> pure (acc [])
            Just x -> go (acc . (x :))

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
    if p t
        then pure t
        else parseError ("satisfy: not satisfied, <" ++ name ++ "> predicate against " ++ show t)

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
