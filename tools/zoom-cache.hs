{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Default
import System.Console.GetOpt
import UI.Command

import Data.ZoomCache.Common
import Data.ZoomCache.Read
import Data.ZoomCache.Write

------------------------------------------------------------

data Config = Config
    { noRaw    :: Bool
    , variable :: Bool
    , intData  :: Bool
    , label    :: L.ByteString
    , rate     :: Integer
    , wmLevel  :: Int
    }

instance Default Config where
    def = defConfig

defConfig :: Config
defConfig = Config
    { noRaw    = False
    , variable = False
    , intData  = False
    , label    = "gen"
    , rate     = 1000
    , wmLevel  = 1024
    }

data Option = NoRaw
            | Variable
            | IntData
            | Label String
            | Rate String
            | Watermark String
    deriving (Eq)

options :: [OptDescr Option]
options = genOptions

genOptions :: [OptDescr Option]
genOptions =
    [ Option ['z'] ["no-raw"] (NoArg NoRaw)
             "Do NOT include raw data in the output"
    , Option ['b'] ["variable"] (NoArg Variable)
             "Generate variable-rate data"
    , Option ['i'] ["integer"] (NoArg IntData)
             "Generate ingeger data"
    , Option ['l'] ["label"] (ReqArg Label "label")
             "Set track label"
    , Option ['r'] ["rate"] (ReqArg Rate "data-rate")
             "Set track rate"
    , Option ['w'] ["watermark"] (ReqArg Rate "watermark")
             "Set high-watermark level"
    ]

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
        processOneOption config Variable = do
            return $ config {variable = True}
        processOneOption config IntData = do
            return $ config {intData = True}
        processOneOption config (Label s) = do
            return $ config {label = LC.pack s}
        processOneOption config (Rate s) = do
            return $ config {rate = read s}
        processOneOption config (Watermark s) = do
            return $ config {wmLevel = read s}

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
    liftIO $ zoomWriteFile config filenames

zoomWriteFile :: Config -> [FilePath] -> IO ()
zoomWriteFile _          []       = return ()
zoomWriteFile Config{..} (path:_)
    | intData   = w ZInt ints path
    | otherwise = w ZDouble doubles path
    where
    w :: (ZoomWrite a, ZoomWrite (TimeStamp, a))
      => TrackType -> [a] -> FilePath -> IO ()
    w ztype d
        | variable  = withFileWrite (oneTrackVariable ztype label)
                          (not noRaw)
                          (sW >> mapM_ (write 1) (zip (map TS [1,3..]) d))
        | otherwise = withFileWrite (oneTrack ztype rate' label)
                          (not noRaw)
                          (sW >> mapM_ (write 1) d)
    rate' = fromInteger rate
    sW = setWatermark 1 wmLevel

------------------------------------------------------------

doubles :: [Double]
doubles = take 1000000 $ map ((* 1000.0) . sin) [0.0, 0.01 ..]

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
