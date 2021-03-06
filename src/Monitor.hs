{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Monitor (monitorServers) where
import Config
import Control.Concurrent
import qualified Control.Monad.Parallel as MP
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Time
import qualified Data.Text.IO as T
import qualified Network.Wreq as WQ
import System.Console.ANSI

type Verbosity    = Bool

class WritableResponse a where
  writeResponse :: a -> IO ()

instance WritableResponse MSAppMonitor where
    writeResponse (MSAppMonitor name url statusKeys) =
        do
          r <- WQ.get url
          mapM_ (printStatus r name) statusKeys

instance WritableResponse SSAppMonitor where
    writeResponse (SSAppMonitor name url) =
        do
          r <- WQ.get url
          printCode r name

printStatus :: JsonResponse -> AppName -> StatusKey -> IO ()
printStatus r n (StatusKey dn jn) =
    do
      setSGR [SetColor Foreground Vivid White]
      putStr $ n ++ " " ++ dn ++ " is: "
      let body = (r ^. WQ.responseBody . key jn . _String)
          color = case body of
                    "OK"      -> Green
                    otherwise -> Red
      setSGR [SetColor Foreground Vivid color]
      T.putStrLn body

printCode :: JsonResponse -> AppName -> IO ()
printCode r n =
    do
      setSGR [SetColor Foreground Vivid White]
      putStr $ n ++ " is: "
      let code = (r ^. WQ.responseStatus . WQ.statusCode)
          color = case code of
                    200       -> Green
                    otherwise -> Red
      setSGR [SetColor Foreground Vivid color]
      Prelude.putStrLn (show code)

getAndPrintServerStatus :: IO ()
getAndPrintServerStatus = do
      cfg <- getConfig
      MP.mapM_ writeResponse $ (ssServers . app) cfg
      MP.mapM_ writeResponse $ (msServers . app) cfg

outWTime :: IO () -> IO ()
outWTime v = do
      t1 <- getCurrentTime
      setSGR [SetColor Foreground Vivid White]
      putStrLn $ "Monitoring apps. Starting pings at: " ++ (show t1)
      v
      t2 <- getCurrentTime
      setSGR [SetColor Foreground Vivid White]
      putStrLn $ "Finished pings at: " ++ (show t2)
      putStrLn $ replicate 100 '-'

monitorServers :: Verbosity -> IO ()
monitorServers v =
    do
      outWTime getAndPrintServerStatus
      threadDelay 5000000
      monitorServers v
