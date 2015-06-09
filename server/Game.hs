module Game where

import Control.Concurrent (MVar, newEmptyMVar, putMVar)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.UUID
import System.Random (randomRIO)

data GameState = GameState { robots :: [Robot], mvar :: MVar GameState }
data Robot = Robot { rId :: UUID, pos :: Position, status :: RobotState, kind :: RobotKind }
    deriving (Eq)
data RobotState = Stopped | Idle | Performing Action
    deriving (Eq)
data Action = Access | Move | Query | Scan | Spawn | Start
    deriving (Eq, Show)
data RobotKind = Headquarters | Engineer | Specialist | Factory
    deriving (Eq, Show)

type Position = (Int, Int)
type Area = (Position, Position)
type Circle = (Position, Int)

-- poor man’s json formatter (for fun!)
jsonifyRobot :: Robot -> String
jsonifyRobot robot = "{" ++ field "type" (kindToString . kind) ++
                     "," ++ field "uuid" (show . rId) ++
                     "," ++ field "x" (show . fst . pos) ++
                     "," ++ field "y" (show . snd . pos) ++
                     "," ++ field "status" (jsonifyState . status) ++
                     "}"
    where field str fun = "\"" ++ str ++ "\":\"" ++ fun robot ++ "\""
          jsonifyState Stopped = "stopped"
          jsonifyState Idle = "idle"
          jsonifyState (Performing action) = firstLower $ show action
          firstLower (x:xs) = toLower x : xs

kindToString :: RobotKind -> String
kindToString = firstLower . show
    where firstLower (c:cs) = toLower c : cs

stringToKind :: String -> Maybe RobotKind
stringToKind "engineer" = Just Engineer
stringToKind "headquarters" = Just Headquarters
stringToKind "specialist" = Just Specialist
stringToKind "factory" = Just Factory
stringToKind _ = Nothing

initialMVar :: IO (MVar GameState)
initialMVar = do
    mvar <- newEmptyMVar
    putMVar mvar (GameState [] mvar)
    return mvar

collision :: Position -> GameState -> Maybe Robot
collision position state = listToMaybe . filter ((== position) . pos) $ robots state

lookupRobot :: UUID -> GameState -> Maybe Robot
lookupRobot ident state = listToMaybe . filter ((== ident) . rId) $ robots state

spawnRobot :: Robot -> GameState -> GameState
spawnRobot robot state = state { robots = robot : robots state }

changeRobot :: UUID -> (Robot -> Robot) -> GameState -> GameState
changeRobot uuid fun state = state { robots = map mapFun $ robots state }
    where mapFun robot
            | rId robot == uuid = fun robot
            | otherwise         = robot

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
distance (x1,y1) (x2,y2)= sqrt $ (fromIntegral x1 - fromIntegral x2) ** 2 + (fromIntegral y1 - fromIntegral y2) ** 2
