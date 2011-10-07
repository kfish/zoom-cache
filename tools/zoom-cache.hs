{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import Data.Default
import System.Console.GetOpt
import UI.Command

import Data.ZoomCache.Common
import Data.ZoomCache.Read
import Data.ZoomCache.Write

------------------------------------------------------------

data Config = Config
    { noRaw :: Bool
    }

instance Default Config where
    def = defConfig

defConfig :: Config
defConfig = Config
    { noRaw = False
    }

data Option = NoRaw
    deriving (Eq)

options :: [OptDescr Option]
options = genOptions

genOptions :: [OptDescr Option]
genOptions = [
    Option ['z'] ["no-raw"] (NoArg NoRaw)
           "Do NOT include raw data in the output" ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
    case getOpt RequireOrder options args of
        (opts, args', [] ) -> do
            config <- processConfig def opts
            return (config, args')
        (_,    _,     _:_) -> return (def, args)

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
    where
        processOneOption config NoRaw = do
            return $ config {noRaw = True}

------------------------------------------------------------

zoomGen :: Command ()
zoomGen = defCmd {
          cmdName = "gen"
        , cmdHandler = zoomGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate floating-point zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zxd", "foo.zxd")]
        }

zoomGenHandler :: App () ()
zoomGenHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ (zoomWriteFile config ZDouble doubles) filenames

zoomWriteFile :: (ZoomWrite a) => Config -> TrackType -> [a] -> [FilePath] -> IO ()
zoomWriteFile _ _     _ []       = return ()
zoomWriteFile Config{..} ztype d (path:_) =
    withFileWrite (oneTrack ztype 1000 "gen")
        (not noRaw) (mapM_ (write 1) d) path

------------------------------------------------------------

zoomGenVariable :: Command ()
zoomGenVariable = defCmd {
          cmdName = "genvbr"
        , cmdHandler = zoomGenVariableHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate variable-bitrate floating-point zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zxd", "foo.zxd")]
        }

zoomGenVariableHandler :: App () ()
zoomGenVariableHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ (zoomWriteFileVariable config ZDouble doubles) filenames

zoomWriteFileVariable :: (ZoomWrite (TimeStamp, a))
                      => Config -> TrackType -> [a] -> [FilePath] -> IO ()
zoomWriteFileVariable _          _     _ []       = return ()
zoomWriteFileVariable Config{..} ztype d (path:_) =
    withFileWrite (oneTrackVariable ztype "gen")
        (not noRaw) (mapM_ (write 1) (zip (map TS [1,3..]) d)) path

doubles :: [Double]
doubles = take 1000000 $ map ((* 1000.0) . sin) [0.0, 0.01 ..]

------------------------------------------------------------

zoomGenI :: Command ()
zoomGenI = defCmd {
          cmdName = "geni"
        , cmdHandler = zoomGenIHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate integer zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zxd", "foo.zxd")]
        }

zoomGenIHandler :: App () ()
zoomGenIHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ (zoomWriteFile config ZInt ints) filenames

------------------------------------------------------------

zoomGenIVariable :: Command ()
zoomGenIVariable = defCmd {
          cmdName = "genivbr"
        , cmdHandler = zoomGenIVariableHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate variable-bitrate integer zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zxd", "foo.zxd")]
        }

zoomGenIVariableHandler :: App () ()
zoomGenIVariableHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ (zoomWriteFileVariable config ZInt	ints) filenames

ints :: [Int]
ints = map round doubles

------------------------------------------------------------

zoomDump :: Command ()
zoomDump = defCmd {
          cmdName = "dump"
        , cmdHandler = zoomDumpHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom-cache data"
        , cmdExamples = [("Yo", "")]
        }

zoomDumpHandler :: App () ()
zoomDumpHandler = liftIO . zoomDumpFile =<< appArgs

------------------------------------------------------------

zoomSummary :: Command ()
zoomSummary = defCmd {
          cmdName = "summary"
        , cmdHandler = zoomSummaryHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom-cache summary data"
        , cmdExamples = [("Read summary level 3 from foo.zxd", "3 foo.zxd")]
        }

zoomSummaryHandler :: App () ()
zoomSummaryHandler = liftIO . f =<< appArgs
    where
        f (lvl:paths) = zoomDumpSummaryLevel (read lvl) paths
        f _ = putStrLn "Usage: zoom-cache summary n file.zxd"

------------------------------------------------------------
-- The Application
--

zoom :: Application () ()
zoom = def {
          appName = "zoom"
        , appVersion = "0.1"
        , appAuthors = ["Conrad Parker"]
        , appBugEmail = "conrad@metadecks.org"
        , appShortDesc = "Trivial zoom-cache inspection tools"
        , appLongDesc = longDesc
        , appCategories = ["Reading", "Writing"]
        , appSeeAlso = [""]
        , appProject = "Zoom"
        , appCmds = [ zoomGen
                    , zoomGenI
                    , zoomGenVariable
                    , zoomGenIVariable
                    , zoomDump
                    , zoomSummary
                    ]
	}

longDesc :: String
longDesc = "Manipulate zoom-cache files"

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
