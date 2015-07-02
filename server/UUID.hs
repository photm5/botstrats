module UUID
( randomUUID
) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import Data.UUID ()
import Data.UUID.V4 (nextRandom)

randomUUID :: IO B.ByteString
randomUUID = (B.pack . show) <$> nextRandom
