module Mutables where

import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Messages (Message)
import Robots (Robot)
import System.IO (Handle)

data Mutables = Mutables { server :: MVar Handle
                         , robots :: MVar [Robot]
                         , waiters :: MVar [Waiter]
                         }

data Waiter = Waiter { awaitedId :: B.ByteString
                     , callback :: Message -> IO ()
                     }

initialMutables :: Handle -> IO Mutables
initialMutables handle = Mutables <$>
    newMVar handle <*> newMVar [] <*> newMVar []

registerWaiter :: MVar [Waiter] -> Waiter -> IO ()
registerWaiter mvar waiter = modifyMVar_ mvar $ return . (waiter:)
