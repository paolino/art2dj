{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Main where


import Prelude hiding (catch)
import Sound.JACK (setOfPorts, clientClose, handleExceptions, withPort, withClientDefault, withActivation, Port, Process, waitForBreak, setProcess, makeProcess, NFrames (..), Input, nframesIndices, nframesBounds)
import Sound.JACK.Audio (getBufferArray, Sample)

import Control.Monad.Trans (lift, liftIO)
import Control.Monad (foldM, forever, when, liftM2)

import Foreign.Ptr (nullPtr, )
import Foreign.C.Error (eOK, )

import System.IO hiding (openFile, hClose, hPutBuf, Handle, IOMode (WriteMode))

import Data.Array.Storable (StorableArray, readArray, writeArray,withStorableArray)

import Sound.File.Sndfile (openFile, hClose, hPutBuf, Handle, IOMode (WriteMode), Info(..)) 
import Data.Array.MArray (newArray_)
import Unsafe.Coerce (unsafeCoerce)
import Control.Concurrent (threadDelay)

import Configuration (lapse, extension, timestamp, fileformat,jackRate)

import Control.Concurrent.STM
import Control.Concurrent
import Data.Monoid (mappend)
import System.Console.Haskeline hiding (bracket, handle)
import Data.Maybe (fromJust)
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
import Control.Exception.Base (AsyncException (..), handle, bracket , throw, throwIO)
import System.Directory (copyFile, getTemporaryDirectory)
import System.FilePath ((</>))

main:: IO ()
main  = do
    -- choose the name of the file
    -- a name for the jack port client
    let name = "art2dj"
    tchandles <- atomically newTChan
    tcproduction <- atomically newTChan
    tcend <- atomically newTChan
    _ <- installHandler keyboardSignal
        (Catch $ atomically $ writeTChan tcproduction Nothing)
        Nothing
    forkIO . runInputT defaultSettings $ interaction tchandles tcproduction tcend
    handleExceptions $
        withClientDefault name $ \client ->
        withPort client "iL" $ \iL ->
        withPort client "iR" $ \iR -> do
            flip (setProcess client) nullPtr =<< lift (makeProcess $ capture tchandles tcproduction iL iR)
            withActivation client $ do
                 lift . atomically $ readTChan tcend

interaction :: TChan Handle -> TChan (Maybe Int) -> TChan () -> InputT IO ()

interaction tchandles tcproduction tcend = do
        let record cond = do 
                liftIO $ do 
                        let       cycle t n handle =  do 
                                        putStr (" _ recording time :" ++ show (t `div` jackRate) ++ " seconds _\r") >> hFlush stdout
                                        atomically $ writeTChan tchandles handle -- schedule recording some frames on handle
                                        ml <- atomically $ readTChan tcproduction -- wait for it
                                        case ml of
                                                Nothing -> return () -- keyboard interrupt asked
                                                Just l -> when (cond $ t + l) $ cycle (t + l) (n + 1) handle -- again
                        tmpname <- liftM2 (</>) getTemporaryDirectory $ timestamp 0
                        let info = Info 0 jackRate 2 fileformat 1 True
                        bracket (openFile tmpname WriteMode info) hClose $ cycle 0 0
                        name <- timestamp 0
                        copyFile tmpname name
                        putStrLn $ "\r**** " ++ name ++ " written. ****                                      "
        recordTimeS <- getInputLine $ "Recording time in seconds: "
        case reads `fmap` recordTimeS of 
                Nothing -> liftIO . atomically $ writeTChan tcend () -- ctrl-d asked 
                Just [] -> outputStrLn ("recording at infinitum ") >> record (const True) >> interaction tchandles tcproduction tcend
                Just [(recordTime,_)] -> do
                        let     recordTimeInt = round recordTime
                                cond t = t < jackRate * recordTimeInt
                        outputStrLn ("recording for " ++ show recordTimeInt ++ " seconds ") >> record cond >> interaction tchandles tcproduction tcend

capture ::  TChan Handle -> TChan (Maybe Int) -> Port Sample Input -> Port Sample Input -> Process a
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
                atomically $ writeTChan tcproduction $ Just count
    return eOK

