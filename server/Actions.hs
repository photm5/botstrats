{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Actions where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import System.IO (Handle())
import System.IO.Error

import Game
import Messages
import UUID

type ActionM = ReaderT (Message, Handle) (ExceptT B.ByteString (StateT GameState IO))
type ActionHandler = [B.ByteString] -> Robot -> ActionM ()

runAction :: Handle -> Message -> GameState -> IO GameState
runAction handle message state = do
    let actionID:robotID:args = parts message
    let robot = lookupRobot robotID state
    let noRequesterError = return (Left "Requester does not exist", state)
    (res, state') <- maybe noRequesterError
        (runTransformers state (message, handle) . action actionID args) robot
    case res of
        Left err -> do
            send handle $ Message (mId message) "result" [actionID, robotID, "failure", err]
            return state
        Right () -> return state'
    where runTransformers state tuple =
            flip runStateT state . runExceptT . flip runReaderT tuple

action :: B.ByteString -> ActionHandler

action "scan" = \args robot -> do
    expect (args == []) "No arguments expected"
    let range = if kind robot == Headquarters then 100 else 10
    delay 1000 Scan $ do
        success "begin_of_stream"
        mapM_ result . map jsonifyRobot . filter ((<= range) . distance (pos robot) . pos) . robots =<< get
        result "end_of_stream"

action "query" = \args robot -> do
    expect (args == []) "No arguments expected"
    success $ jsonifyRobot robot

action "spawn" = \args robot -> do
    expect (length args == 1 || length args == 3) "Expected one or three arguments"
    expect (isJust . stringToKind . head $ args) "Invalid robot type"
    let targetKind = fromJust . stringToKind . head $ args
    expect ((kind robot, targetKind) `elem` (join
        [ (Headquarters,) <$> [Engineer, Specialist]
        , (Factory,)      <$> [Engineer]
        , (Engineer,)     <$> [Factory]
        ])) "Your robot type is not allowed to spawn that type of robot"
    position <- case args of
                [_] -> liftIO $ randomPositionInCircle (pos robot, 5)
                [_, x, y] ->
                    catchIO (read $ B.unpack x, read $ B.unpack y) . const $ throwError "Bad Number"
    delay 1000 Spawn $ do
        uuid <- liftIO $ randomUUID
        modify . spawnRobot $ Robot uuid position Off targetKind
        (_, handle) <- ask
        liftIO $ newMessage "spawn" [(kindToString targetKind), uuid]
            >>= send handle
        success uuid

action "move" = \args robot -> do
    expect (length args == 1) "Expected one argument"
    case stringToDirection $ head args of
        Nothing -> throwError "Invalid direction"
        Just direction -> do
            st <- get
            case collision (move direction $ pos robot) st of
                Nothing -> delay 1000 Move $ do
                    modify . changeRobot (rId robot) $ moveRobot direction
                    success ""
                Just _ -> throwError "Target position occupied"

action "start" = \args robot -> do
    expect (length args == 1) "Expected one argument"
    let targetID = head args
    target <- gets $ lookupRobot targetID
    expect (isJust target) "No such target"
    delay 1000 Start $ do
        setStatus targetID Idle
        (_, handle) <- ask
        liftIO $ newMessage "start" [targetID] >>= send handle
        success ""

action _ = \_ _ -> throwError "No such action"

expect :: (MonadError e m) => Bool -> e -> m ()
expect bool err
    | bool == True = return ()
    | otherwise    = throwError err

catchIO :: (MonadError e m, MonadIO m) => a -> (IOError -> m a) -> m a
catchIO x fun = (liftIO . tryIOError $ return x) >>= either fun return

success :: B.ByteString -> ActionM ()
success "" = result "success"
success msg = result $ "success " <> msg

result :: B.ByteString -> ActionM ()
result msg = do
    (message, handle) <- ask
    let action:robotID:args = parts message
    liftIO . send handle $ Message (mId message) "result" [action, robotID, msg]

delay :: Int -> Action -> ActionM () -> ActionM ()
delay cost action runAction = do
    (message, handle) <- ask
    let actionID:robotID:args = parts message
    setStatus robotID $ Performing action
    stateVar <- mvar <$> get
    void . liftIO . forkIO $ do
        threadDelay $ cost * 1000
        modifyMVar_ stateVar $ \state -> do
            (res, state') <- flip runStateT state . runExceptT . flip runReaderT (message, handle) $ do 
                runAction
                setStatus robotID Idle
            case res of
                Left err -> do
                    send handle $ Message (mId message) "result" [actionID, robotID, "failure", err]
                    return state
                Right () -> return state'
