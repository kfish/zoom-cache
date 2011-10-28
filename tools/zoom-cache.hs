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

import Data.ZoomCache
import Data.ZoomCache.Dump

------------------------------------------------------------

data Config = Config
    { noRaw    :: Bool
    , variable :: Bool
    , intData  :: Bool
    , label    :: L.ByteString
    , rate     :: Integer
    , wmLevel  :: Int
    , track    :: TrackNo
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
    , track    = 1
    }

data Option = NoRaw
            | Variable
            | IntData
            | Label String
            | Rate String
            | Watermark String
            | Track String
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
    , Option ['t'] ["track"] (ReqArg Track "trackNo")
             "Set or select track number"
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
        processOneOption config (Track s) = do
            return $ config {track = read s}

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
        | variable  = withFileWrite (oneTrack ztype VariableDR rate' label)
                          (not noRaw)
                          (sW >> mapM_ (write track) (zip (map TS [1,3..]) d))
        | otherwise = withFileWrite (oneTrack ztype ConstantDR rate' label)
                          (not noRaw)
                          (sW >> mapM_ (write track) d)
    rate' = fromInteger rate
    sW = setWatermark 1 wmLevel

------------------------------------------------------------

doubles :: [Double]
doubles = take 10000000 $ map ((* 1000.0) . sin) [0.0, 0.01 ..]

ints :: [Int]
ints = map round doubles

------------------------------------------------------------

zoomInfo :: Command ()
zoomInfo = defCmd {
          cmdName = "info"
        , cmdHandler = zoomInfoHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Display basic info about a zoom-cache file"
        , cmdExamples = [("Display info about foo.zxd", "foo.zxd")]
        }

zoomInfoHandler :: App () ()
zoomInfoHandler = mapM_ (liftIO . zoomInfoFile) =<< appArgs

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
zoomDumpHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    mapM_ (liftIO . zoomDumpFile (track config)) filenames

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
zoomSummaryHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO . (f (track config)) $ filenames
    where
        f trackNo (lvl:paths) = mapM_ (zoomDumpSummaryLevel trackNo (read lvl)) paths
        f _ _ = putStrLn "Usage: zoom-cache summary n file.zxd"

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
                    , zoomInfo
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
