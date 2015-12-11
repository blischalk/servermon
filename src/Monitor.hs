{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Monitor (monitorServers) where
import Config
import Control.Concurrent
import Control.Lens
import Data.Aeson.Lens (_String, key)
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
      setSGR [SetColor Foreground Vivid Green]
      T.putStrLn (r ^. WQ.responseBody . key jn . _String)

printCode :: JsonResponse -> AppName -> IO ()
printCode r n =
    do
      setSGR [SetColor Foreground Vivid White]
      putStr $ n ++ " is: "
      setSGR [SetColor Foreground Vivid Green]
      Prelude.putStrLn (show (r ^. WQ.responseStatus . WQ.statusCode))

monitorServers :: Verbosity -> IO ()
monitorServers v =
    do
      cfg <- getConfig
      mapM_ writeResponse $ (ssServers . app) cfg
      mapM_ writeResponse $ (msServers . app) cfg
      threadDelay 5000000
      monitorServers v
