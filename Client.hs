import Prelude hiding (writeFile)
import Data.ByteString.Lazy hiding (map, putStrLn, putStr)
import Data.ByteString.Internal
import Network.HTTP
import Control.Monad (when)
import Data.Time.Clock
import System.FilePath
import System.IO hiding (writeFile)
import Control.Concurrent
import Configuration


bsRq (Request x y z k) = Request x y z (pack $ map c2w k)


file x = do 
        r <- simpleHTTP (bsRq . getRequest $ server ++ x)
        case r of 
                Right r -> let  b = rspBody r
                                c = rspCode r
                           in case c of 
                                (4,0,4) -> putStr (x ++ " not found on " ++ server ++ "\r") >> hFlush stdout
                                _ -> putStrLn ("\n" ++ "writing file " ++ x) >> writeFile (path </> x) b
                Left e -> putStrLn $ "\n" ++ x ++ " errore " ++ server ++ ":" ++ show e

thread r = do
        threadDelay $ polling * 10 ^ 6
        t <- ((`div` lapse) . truncate . utctDayTime) `fmap` getCurrentTime 
        when (t > r) $ sequence_ [file (show z ++ extension) | z <- [r + 1 .. t]]
        thread t

main = thread 0
