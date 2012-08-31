{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Main where

import Sound.JACK (handleExceptions, withPort, withClientDefault, withActivation, Port, Process, waitForBreak, setProcess, makeProcess, NFrames (..), Input, nframesIndices, nframesBounds)
import Sound.JACK.Audio (getBufferArray, Sample)

import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_, forever, when)

import Foreign.C.Error (eOK)
import Foreign (sizeOf)
import Foreign.Storable (sizeOf, peek, )
import Foreign.Ptr (nullPtr, )
import Foreign.C.Error (eOK, )

import System.Environment (getProgName)
import System.IO

import Data.Array.Storable (StorableArray, readArray, writeArray,withStorableArray)

-- import Sound.File.Sndfile (openFile, hClose, hPutBuf, Handle, IOMode (WriteMode), Info (format), Format (sampleFormat) , SampleFormat (SampleFormatFloat)) 
import Data.Array.MArray (newArray_)
import Unsafe.Coerce (unsafeCoerce)
import Control.Concurrent (threadDelay)

import Data.Time.Clock(getCurrentTime , utctDayTime)
import Configuration (lapse, extension, recordTime)
import Data.Ix (rangeSize)


import Control.Concurrent.STM
import Control.Concurrent
import Sound.Sox.Option.Format (none, numberOfChannels, sampleRate)
import Sound.Sox.Convert (simple)
import Data.Monoid (mappend)

import Data.Array.Base (getNumElements, )

-- infoOutput = Info 0 44100 1 (Format HeaderFormatAiff  EndianFile ) 1 True

rawextension = ".f32"

main:: IO ()
main  = do
    -- choose the name of the file
    -- a name for the jack port client
    let name = "jack capture " ++ show recordTime
    tchandles <- atomically $ newTChan
    tcproduction <- atomically $ newTChan
    t <- forkIO . forever $ interaction tchandles tcproduction
    handleExceptions $
        withClientDefault name $ \client ->
        withPort client "inputLeft" $ \inputLeft ->
        withPort client "inputRight" $ \inputRight -> do
            flip (setProcess client) nullPtr =<< lift (makeProcess $ capture tchandles tcproduction inputLeft inputRight)
            withActivation client . lift $ 
                threadDelay maxBound
    killThread t

interaction tchandles tcproduction = do
        putStrLn $ "Press return for record next " ++ show lapse ++ " seconds"
        getLine 
        now <- ((`div` lapse) . truncate . utctDayTime) `fmap` getCurrentTime 
        let delta = recordTime `div` lapse + 1
        let name = show (now + delta)
        putStrLn $ "Writing " ++ name ++ rawextension
        let     cond t = t < 44100 * recordTime
                cycle t n handle =  if cond t then do 
                        when (n `mod` 10 == 0) $ do 
                                putStr "."
                                hFlush stdout
                        atomically $ writeTChan tchandles handle
                        l <- atomically $ readTChan tcproduction
                        cycle (t + l) (n + 1) handle
                        else putStrLn "" 
        withBinaryFile  (name ++ rawextension)  WriteMode $ cycle 0 0  
        putStr $ "Writing " ++ name ++ extension
        hFlush stdout
        r <- simple (numberOfChannels 2 `mappend` sampleRate 44100)  (name ++ rawextension) none (name ++ extension) >> return ()
        putStrLn $ " " ++ show r
        interaction tchandles tcproduction


capture ::  TChan Handle -> TChan Int -> Port Sample Input -> Port Sample Input -> Process a
capture tchandle tcproduction iL iR nframes _args = do
    inLArr <- getBufferArray iL nframes
    inRArr <- getBufferArray iR nframes
    let (a,b) = nframesBounds nframes
    output <- newArray_ ((a,0),(b,1))
    mh <- atomically (Just `fmap` readTChan tchandle `orElse` return Nothing)
    case mh of 
        Nothing -> return ()
        Just handle ->  do
                forM_ (nframesIndices nframes) $ \i -> do
                                el <- readArray inLArr i
                                er <- readArray inRArr i
                                writeArray output (i,0) el
                                writeArray output (i,1) er                                
                n <- getNumElements output
                withStorableArray output $ \p -> do
                        dummy <- peek p
                        hPutBuf handle p $ sizeOf dummy * n
                atomically (writeTChan tcproduction $ n `div` 2)
    return eOK

