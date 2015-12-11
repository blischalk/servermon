{-# LANGUAGE DeriveGeneric #-}
module Config (
getConfig
, JsonResponse(..)
, AppName(..)
, StatusKey(..)
, SSAppMonitor(..)
, MSAppMonitor(..)
, MyConfig(..)
, AppConfig(..)) where

import Control.Applicative
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text.Internal as TI
import Data.Yaml
import qualified Network.Wreq as WQ
import GHC.Generics
import System.Environment

type AppName      = String
type AppUrl       = String
type DisplayName  = String
type JsonName     = TI.Text
type JsonResponse = WQ.Response BS.ByteString

data StatusKey    = StatusKey { displayName :: DisplayName
                              , jName       :: JsonName
                              } deriving (Show, Generic)

data MSAppMonitor = MSAppMonitor { msName       :: AppName
                                 , msUrl        :: AppUrl
                                 , msStatusKeys :: [StatusKey]
                                 } deriving (Show, Generic)

data SSAppMonitor = SSAppMonitor { ssName :: AppName
                                 , ssUrl  :: AppUrl
                                 } deriving (Show, Generic)

data MyConfig = MyConfig { app :: AppConfig } deriving (Show, Generic)

data AppConfig = AppConfig { ssServers :: [SSAppMonitor]
                           , msServers :: [MSAppMonitor]
                           } deriving (Show, Generic)

instance FromJSON SSAppMonitor
instance ToJSON SSAppMonitor
instance FromJSON MSAppMonitor
instance ToJSON MSAppMonitor
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
