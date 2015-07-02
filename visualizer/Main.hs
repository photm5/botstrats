module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Data.Maybe (isJust, fromJust, listToMaybe)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Network
import Networking
import System.IO (hGetContents)

type Position = (Float, Float)
type Dimensions = (Float, Float)
type TimeSpan = Float
type TimePoint = Float

data Image = Image { identifier :: String
                   , picture :: Picture
                   , bornAt :: TimePoint
                   }

data Message = Message { mIdentifier :: String
                       , mImageName :: String
                       , mPosition :: Position
                       , mDimensions :: Dimensions
                       , mLifeTime :: Maybe TimeSpan
                       }

displayMode = InWindow "botstrats visualizer" (800, 600) (0, 0)
resourcePath = "../resources/"
imageSize = 500

frameFun :: MVar [Image] -> TimePoint -> IO Picture
frameFun mvar float = withMVar mvar $ return . pictures . map picture

parseLine :: String -> Maybe Message
parseLine = Just . parseLine' . words

parseLine' :: [String] -> Message
parseLine' split = Message uuid name (read x, read y) (read w, read h) (parseTime <$> listToMaybe rest)
    where (uuid : name : w : h : x : y : rest) = split

parseTime :: String -> TimeSpan
parseTime str = fromRational $ read str / 10


networkLoop :: MVar [Image] -> MVar TimePoint -> PortNumber -> IO ()
networkLoop images timeVar p = listenLoop (PortNumber p) $ \handle hostname port -> do
        contents <- hGetContents handle
        forM_ (lines contents) $ \line -> do
            putStrLn line
            case parseLine line of
                Nothing -> putStrLn "parse failed"
                Just msg -> handleMessage images timeVar msg

handleMessage :: MVar [Image] -> MVar TimePoint -> Message -> IO ()
handleMessage images timeVar msg = do
    img <- loadBMP $ resourcePath ++ (mImageName msg) ++ ".bmp"
    let pic = scaleWith msg . moveWith msg $ img
    withMVar timeVar $ \now ->
        modifyMVar_ images $ \images ->
            return $ override (Image (mIdentifier msg) pic now) images

scaleWith :: Message -> Picture -> Picture
scaleWith message = scale x y
    where (x, y) = mDimensions message

moveWith :: Message -> Picture -> Picture
moveWith message = translate (x * imageSize) (y * imageSize)
    where (x, y) = mPosition message

main = do
    images <- newMVar []
    time <- newMVar 0
    forkIO $ networkLoop images time 2000
    animateIO displayMode black $ \curTime -> do
        swapMVar time curTime
        frameFun images curTime

override :: Image -> [Image] -> [Image]
override x xs
    | any eq xs = (map (const x) . filter eq $ xs) ++ filter (not . eq) xs
    | otherwise = x : xs
    where eq i = identifier x == identifier i
