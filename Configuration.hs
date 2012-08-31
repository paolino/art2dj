{-# LANGUAGE NoMonomorphismRestriction #-}
module Configuration where


-- client parameters
server = "http://127.0.0.1/"
clientPath = "."
polling = 3

-- server parameters
recordTime = 10
jackRate = 44100

-- common parameters
-- quantum of space between timestamps in seconds
lapse = 10
-- extension to be appended to timestamps name
extension = ".mp3"

