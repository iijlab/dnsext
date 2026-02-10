module Axfr where

import DNS.Auth.Algorithm
import DNS.Do53.Internal
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode

import Network.Socket

axfr :: DB -> Socket -> IO ()
axfr db sock = do
    equery <- decode <$> recvVC (32 * 1024) (recvTCP sock)
    case equery of
        Left _ -> return ()
        Right query -> do
            let reply
                    | qtype (question query) == AXFR =
                        makeResponse
                            (identifier query)
                            (question query)
                            (dbAll db)
                    | otherwise = getAnswer db query
            sendVC (sendTCP sock) $ encode reply
