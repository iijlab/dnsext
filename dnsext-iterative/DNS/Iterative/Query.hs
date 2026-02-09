module DNS.Iterative.Query (
    -- * Env, Types
    module DNS.Iterative.Query.Env,
    module DNS.Iterative.Query.Types,

    -- * Iterative query
    resolveResponseIterative,
    foldResponseIterative,
    foldResponseIterative',

    -- * Cache
    foldResponseCached,
) where

import DNS.Do53.Client
import DNS.Iterative.Query.API
import DNS.Iterative.Query.Env
import DNS.Iterative.Query.Types (VResult (..))
import DNS.Types

resolveResponseIterative :: Env -> Question -> QueryControls -> IO (Either String DNSMessage)
resolveResponseIterative env q ictl = foldResponseIterative' Left (\_ -> Right) env 0 {- dummy id -} q ictl
