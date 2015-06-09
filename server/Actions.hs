{-# LANGUAGE TupleSections #-}

module Actions where

import Data.Maybe (isJust, fromJust)
import Data.UUID
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import System.IO (Handle(), hPutStrLn)
import System.IO.Error

import Game
import Messages

type ActionHandler = [String] -> Robot -> ReaderT (Message, Handle) (ExceptT String (StateT GameState IO)) ()

runAction :: Handle -> Message -> GameState -> IO GameState
runAction handle message state = do
    let actionID:robotID:args = parts message
    let robot = lookupRobot (read robotID) state
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

action :: String -> ActionHandler

action "scan" = \args robot -> do
    expect (args == []) "No arguments expected"
    let range = if kind robot == Headquarters then 100 else 10
    let cost  = if kind robot == Headquarters then 0 else 10
    success "begin_of_stream"
    mapM_ result . map jsonifyRobot . robots =<< get
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
                    catchIO (read x, read y) . const $ throwError "Bad Number"
    throwError $ "NYI"

action "move" = \args robot -> do
    expect (length args == 1) "Expected one argument"
    throwError "NYI"

action "start" = \args robot -> do
    expect (length args == 1) "Expected one argument"
    let targetID = fromString $ head args
    expect (isJust targetID) "Invalid uuid"
    target <- gets . lookupRobot $ fromJust targetID
    expect (isJust target) "No such target"
    throwError "NYI"

action _ = \_ _ -> throwError "No such action"

expect :: (MonadError e m) => Bool -> e -> m ()
expect bool err
    | bool == True = return ()
    | otherwise    = throwError err

catchIO :: (MonadError e m, MonadIO m) => a -> (IOError -> m a) -> m a
catchIO x fun = (liftIO . tryIOError $ return x) >>= either fun return

success :: String -> ReaderT (Message, Handle) (ExceptT String (StateT GameState IO)) ()
success msg = result $ "success " ++ msg

result :: String -> ReaderT (Message, Handle) (ExceptT String (StateT GameState IO)) ()
result msg = do
    (message, handle) <- ask
    let action:robotID:args = parts message
    liftIO . send handle $ Message (mId message) "result" [action, robotID, msg]

{-
performCost :: (MonadIO m) => Float -> m () -> m ()
performCost cost m = if cost == 0 then m else void . liftIO . forkIO $ do
    liftIO . threadDelay . round $ cost * 1000
    m
    -}
