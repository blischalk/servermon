{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Monitor (monitorServers) where
import Control.Applicative
import Control.Lens
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text.Internal as TI
import qualified Data.Text.IO as T
import Data.Yaml
import GHC.Generics
import qualified Network.Wreq as WQ
import Prelude hiding (lookup)
import System.Console.ANSI
import System.Environment

type Verbosity    = Bool
type AppURLList   = [AppMonitor]
type AppName      = String
type AppUrl       = String
type DisplayName  = String
type JsonName     = TI.Text
type JsonResponse = WQ.Response BS.ByteString

data StatusKey    = StatusKey { displayName :: DisplayName
                              , jName       :: JsonName
                              } deriving (Show, Generic)

data AppMonitor  = AppMonitor { name       :: AppName
                              , url        :: AppUrl
                              , statusKeys :: [StatusKey]
                              } deriving (Show, Generic)

data MyConfig = MyConfig { app :: AppConfig } deriving (Show, Generic)

data AppConfig = AppConfig { servers :: [AppMonitor] } deriving (Show, Generic)

instance FromJSON AppMonitor
instance ToJSON AppMonitor
instance FromJSON StatusKey
instance ToJSON StatusKey
instance FromJSON MyConfig
instance ToJSON MyConfig
instance FromJSON AppConfig
instance ToJSON AppConfig

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

readMyConfig :: String -> IO MyConfig
readMyConfig s =
    either (error . show) id <$>
    decodeFileEither s

monitorServers :: Verbosity -> IO ()
monitorServers v =
    do
      cfgPath <- getEnv "SM_CONFIG_PATH"
      config <- readMyConfig $ cfgPath ++ "/servers.yaml"
      mapM_ writeResponse $ (servers . app) config
      monitorServers v
