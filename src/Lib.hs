{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parseArgs
    ) where

import Control.Lens
import Control.Monad
import Data.Aeson.Lens (_String, key)
import qualified Network.Wreq as WQ
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.Text.IO as T

data Options = Options { optVerbose :: Bool }

startOptions :: Options
startOptions = Options { optVerbose = False }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "v" ["verbose"]
      (NoArg
       (\opt -> return opt { optVerbose = True }))
      "Enable verbose messages"
    , Option "V" ["version"]
      (NoArg
       (\_ -> do
          hPutStrLn stderr "Version 0.01"
          exitWith ExitSuccess))
      "Print version"
    , Option "h" ["help"]
      (NoArg
       (\_ -> do
          prg <- getProgName
          hPutStrLn stderr (usageInfo prg options)
          exitWith ExitSuccess))
      "Show help"
    ]

parseArgs :: IO ()
parseArgs =
    do
      args <- getArgs
      let (actions, nonOptions, errors) = getOpt RequireOrder options args
      opts <- foldl (>>=) (return startOptions) actions
      let Options { optVerbose = verbose } = opts
      r <- WQ.get "http://httpbin.org/get"
      T.putStrLn (r ^. WQ.responseBody . key "url" . _String)
      when verbose (hPutStrLn stderr "Starting in verbose mode...")
