module Lib
    ( parseArgs
    ) where

import Control.Monad
import Monitor
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

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
      when verbose (hPutStrLn stderr "Starting in verbose mode...")
      monitorServers verbose
