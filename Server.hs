{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Main where

import Sound.JACK (setOfPorts, clientClose, handleExceptions, withPort, withClientDefault, withActivation, Port, Process, waitForBreak, setProcess, makeProcess, NFrames (..), Input, nframesIndices, nframesBounds)
import Sound.JACK.Audio (getBufferArray, Sample)

import Control.Monad.Trans (lift, liftIO)
import Control.Monad (foldM, forever, when)

import Foreign.Ptr (nullPtr, )
import Foreign.C.Error (eOK, )

import System.IO hiding (openFile, hClose, hPutBuf, Handle, IOMode (WriteMode))

import Data.Array.Storable (StorableArray, readArray, writeArray,withStorableArray)

import Sound.File.Sndfile (openFile, hClose, hPutBuf, Handle, IOMode (WriteMode), Info(..)) 
import Data.Array.MArray (newArray_)
import Unsafe.Coerce (unsafeCoerce)
import Control.Concurrent (threadDelay)

import Data.Time -- (getCurrentTime , utctDayTime, UTCTime (..))
import Configuration (lapse, extension, recordTime, fileformat,jackRate)

import Control.Concurrent.STM
import Control.Concurrent
import Data.Monoid (mappend)
import Data.Time.Lens
import Data.Lens.Lazy
import Text.Printf
import System.Console.Haskeline
import Data.Maybe (fromJust)

main:: IO ()
main  = do
    -- choose the name of the file
    -- a name for the jack port client
    let name = "art2dj"
    tchandles <- atomically newTChan
    tcproduction <- atomically newTChan
    tcend <- atomically newTChan
    forkIO . runInputT defaultSettings $ interaction tchandles tcproduction tcend
    handleExceptions $
        withClientDefault name $ \client ->
        withPort client "iL" $ \iL ->
        withPort client "iR" $ \iR -> do
            flip (setProcess client) nullPtr =<< lift (makeProcess $ capture tchandles tcproduction iL iR)
            withActivation client $ do
                 lift . atomically $ readTChan tcend
                 clientClose client $ setOfPorts [iL,iR]
timestamp t = printf "%02d-%02d-%02d %02d:%02d:%02d" (day ^$ t) (month ^$ t) (year ^$ t) (hours ^$ t) (minutes ^$ t) (truncate $ (seconds ^$ t) :: Int)
interaction :: TChan Handle -> TChan Int -> TChan () -> InputT IO ()

interaction tchandles tcproduction tcend = do
        recordTimeS <- getInputLine $ "Number of seconds to record: "
        case reads `fmap` recordTimeS of 
                Nothing -> liftIO . atomically $ writeTChan tcend ()
                Just [] -> outputStr (fromJust recordTimeS ++ "is not a number! ") >> interaction tchandles tcproduction tcend
                Just [(recordTime,_)] -> do 
                        liftIO $ do 
                                t <- getZonedTime 
                                let name = timestamp t
                                let     cond t = t < jackRate * recordTime
                                        cycle t n handle =  if cond t then do 
                                                putStr $ " " ++ name ++ extension ++ ": " ++ (show $ 100 * t `div` jackRate `div` recordTime) ++ "%  \r"
                                                hFlush stdout
                                                atomically $ writeTChan tchandles handle
                                                l <- atomically $ readTChan tcproduction
                                                cycle (t + l) (n + 1) handle
                                                else putStrLn $ name ++ extension ++ " written." 
                                handle <- openFile (name ++ extension) WriteMode $ Info 0 jackRate 2 fileformat 1 True
                                cycle 0 0 handle
                                hClose handle
                        interaction tchandles tcproduction tcend


capture ::  TChan Handle -> TChan Int -> Port Sample Input -> Port Sample Input -> Process a
capture tchandle tcproduction iL iR nframes _args = do
    inLArr <- getBufferArray iL nframes
    inRArr <- getBufferArray iR nframes
    let (a,b) = nframesBounds nframes
    output <- newArray_ ((a,0),(b,1)) :: IO (StorableArray (NFrames,Int) Float)
    mh <- atomically (Just `fmap` readTChan tchandle `orElse` return Nothing)
    case mh of 
        Nothing -> return ()
        Just handle ->  do
                count <- (\f -> foldM f 0 (nframesIndices nframes)) $ \c i -> do
                                readArray inLArr i >>= writeArray output (i,0) . realToFrac
                                readArray inRArr i >>= writeArray output (i,1) . realToFrac
                                return $ c + 1
                withStorableArray output $ flip (hPutBuf handle) count
                atomically $ writeTChan tcproduction count
    return eOK

