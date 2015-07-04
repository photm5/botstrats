{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Data.Monoid ((<>))
import Control.Concurrent (MVar, newEmptyMVar, putMVar)
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import System.Random (randomRIO)

data GameState = GameState { robots :: [Robot], mvar :: MVar GameState }
data Robot = Robot { rId :: ID, pos :: Position, status :: RobotState, kind :: RobotKind }
    deriving (Eq)
data RobotState = Off | Idle | Performing Action
    deriving (Eq)
data Action = Access | Move | Query | Scan | Spawn | Start
    deriving (Eq, Show)
data RobotKind = Headquarters | Engineer | Specialist | Factory
    deriving (Eq, Show)
data Direction = North | East | South | West

type Position = (Int, Int)
type Area = (Position, Position)
type Circle = (Position, Int)
type ID = B.ByteString

-- poor man’s json formatter (for fun!)
jsonifyRobot :: Robot -> B.ByteString
jsonifyRobot robot = "{" <> field "type" (kindToString . kind) <>
                     "," <> field "uuid" rId <>
                     "," <> field "x" (B.pack . show . fst . pos) <>
                     "," <> field "y" (B.pack . show . snd . pos) <>
                     "," <> field "status" (jsonifyState . status) <>
                     "}"
    where field str fun = "\"" <> str <> "\":\"" <> fun robot <> "\""
          jsonifyState Off = "off"
          jsonifyState Idle = "idle"
          jsonifyState (Performing action) = firstLower . B.pack $ show action

kindToString :: RobotKind -> B.ByteString
kindToString = firstLower . B.pack . show

firstLower :: B.ByteString -> B.ByteString
firstLower str = (B.singleton . toLower $ B.head str) <> B.tail str

stringToKind :: B.ByteString -> Maybe RobotKind
stringToKind "engineer" = Just Engineer
stringToKind "headquarters" = Just Headquarters
stringToKind "specialist" = Just Specialist
stringToKind "factory" = Just Factory
stringToKind _ = Nothing

stringToDirection :: B.ByteString -> Maybe Direction
stringToDirection "north" = Just North
stringToDirection "east"  = Just East
stringToDirection "west"  = Just West
stringToDirection "south" = Just South
stringToDirection _       = Nothing

initialMVar :: IO (MVar GameState)
initialMVar = do
    mvar <- newEmptyMVar
    putMVar mvar (GameState [] mvar)
    return mvar

collision :: Position -> GameState -> Maybe Robot
collision position state = listToMaybe . filter ((== position) . pos) $ robots state

lookupRobot :: ID -> GameState -> Maybe Robot
lookupRobot ident state = listToMaybe . filter ((== ident) . rId) $ robots state

spawnRobot :: Robot -> GameState -> GameState
spawnRobot robot state = state { robots = robot : robots state }

changeRobot :: ID -> (Robot -> Robot) -> GameState -> GameState
changeRobot uuid fun state = state { robots = map mapFun $ robots state }
    where mapFun robot
            | rId robot == uuid = fun robot
            | otherwise         = robot

move :: Direction -> Position -> Position
move dir (x, y) = (x + dx, y + dy)
    where dx = case dir of
            East -> 1
            West -> -1
            _ -> 0
          dy = case dir of
            North -> 1
            South -> -1
            _ -> 0

moveRobot :: Direction -> Robot -> Robot
moveRobot dir rob = rob { pos = move dir $ pos rob }

setStatus :: (MonadState GameState m) => ID -> RobotState -> m ()
setStatus robotID status = modify . changeRobot robotID $ \r -> r { status = status }

randomPosition :: Area -> IO Position
randomPosition ((x1,y1),(x2,y2))= do
    x <- randomRIO (x1, x2)
    y <- randomRIO (y1, y2)
    return (x, y)

randomPositionInCircle :: Circle -> IO Position
randomPositionInCircle arg@((x,y), r) = do
    pos <- randomPosition ((x - r, y - r), (x + r, y + r))
    if distance (x,y) pos <= fromIntegral r
    then return pos
    else randomPositionInCircle arg

distance :: (Floating a) => Position -> Position -> a
distance (x1,y1) (x2,y2)= sqrt $ (fromIntegral x1 - fromIntegral x2) ** 2
                               + (fromIntegral y1 - fromIntegral y2) ** 2
