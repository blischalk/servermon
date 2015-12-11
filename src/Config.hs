{-# LANGUAGE DeriveGeneric #-}
module Config (
getConfig
, JsonResponse(..)
, AppName(..)
, StatusKey(..)
, AppMonitor(..)
, MyConfig(..)
, AppConfig(..)) where

import Control.Applicative
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text.Internal as TI
import Data.Yaml
import qualified Network.Wreq as WQ
import GHC.Generics
import System.Environment

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

readConfig :: IO (String)
readConfig = do
  cfgPath <- getEnv "SM_CONFIG_PATH"
  return (cfgPath ++ "/servers.yaml")

getConfig :: IO MyConfig
getConfig = do
  cfg <- readConfig
  either (error . show) id <$> decodeFileEither cfg
