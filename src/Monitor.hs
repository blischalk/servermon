{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Monitor (monitorServers) where
import Config
import Control.Lens
import Data.Aeson.Lens (_String, key)
import qualified Data.Text.IO as T
import qualified Network.Wreq as WQ
import System.Console.ANSI

type Verbosity    = Bool

printStatus :: JsonResponse -> AppName -> StatusKey -> IO ()
printStatus r n (StatusKey dn jn) =
    do
      setSGR [SetColor Foreground Vivid White]
      putStr $ n ++ " " ++ dn ++ " is: "
      setSGR [SetColor Foreground Vivid Green]
      T.putStrLn (r ^. WQ.responseBody . key jn . _String)

writeResponse :: AppMonitor -> IO ()
writeResponse (AppMonitor name url statusKeys) =
    do
      r <- WQ.get url
      mapM_ (printStatus r name) statusKeys

monitorServers :: Verbosity -> IO ()
monitorServers v =
    do
      cfg <- getConfig
      mapM_ writeResponse $ (servers . app) cfg
      monitorServers v
