{-# LANGUAGE NoMonomorphismRestriction #-}
module Configuration where

import Data.Lens.Lazy
import Text.Printf
import Data.Time -- (getCurrentTime , utctDayTime, UTCTime (..))
import Data.Time.Lens
import Sound.File.Sndfile
import Data.Fixed

-- client parameters
server = "http://127.0.0.1/"
clientPath = "."
polling = 3

-- server parameters
recordTime = 10
jackRate = 44100
fileformat = Format HeaderFormatAiff  SampleFormatFloat EndianFile 

-- common parameters
-- quantum of space between timestamps in seconds
lapse = 10
-- extension to be appended to timestamps name
extension = ".aiff"

showTime t = printf ("%02d-%02d-%02d-%02d:%02d" ++ extension) (day ^$ t) (month ^$ t) (year ^$ t) (hours ^$ t) (minutes ^$ t)


timestamps :: Int -> IO [String]
timestamps n = reverse `fmap` map showTime `fmap` (\t -> map (\n -> minutes ^%= subtract n $ t ) [0 .. n - 1]) `fmap` getZonedTime

timestamp :: Pico -> IO String
timestamp q = showTime `fmap` (seconds ^%= (+q)) `fmap` getZonedTime
