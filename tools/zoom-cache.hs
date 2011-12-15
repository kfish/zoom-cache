{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as C
import Data.Default
import qualified Data.IntMap as IM
import Data.Time.Clock (getCurrentTime)
import System.Console.GetOpt
import UI.Command

import Data.ZoomCache
import Data.ZoomCache.Dump
import Data.ZoomCache.Multichannel()

------------------------------------------------------------

data Config = Config
    { noRaw    :: Bool
    , channels :: Int
    , wmLevel  :: Int
    , track    :: TrackNo
    , intData  :: Bool
    , variable :: Bool
    , spec     :: TrackSpec
    }

instance Default Config where
    def = defConfig

defConfig :: Config
defConfig = Config
    { noRaw    = False
    , channels = 1
    , wmLevel  = 1024
    , track    = 1
    , intData  = False
    , variable = False
    , spec     = def { specDeltaEncode = False
                     , specZlibCompress = False
                     , specName = "gen"
                     }
    }

data Option = NoRaw
            | Channels String
            | Watermark String
            | Track String
            | Delta
            | ZLib
            | Variable
            | IntData
            | Rate String
            | Label String
    deriving (Eq)

options :: [OptDescr Option]
options = genOptions

genOptions :: [OptDescr Option]
genOptions =
    [ Option ['z'] ["no-raw"] (NoArg NoRaw)
             "Do NOT include raw data in the output"
    , Option ['c'] ["channels"] (ReqArg Channels "channels")
             "Set number of channels"
    , Option ['w'] ["watermark"] (ReqArg Watermark "watermark")
             "Set high-watermark level"
    , Option ['t'] ["track"] (ReqArg Track "trackNo")
             "Set or select track number"
    , Option ['d'] ["delta"] (NoArg Delta)
             "Delta-encode data"
    , Option ['Z'] ["zlib"] (NoArg ZLib)
             "Zlib-compress data"
    , Option ['b'] ["variable"] (NoArg Variable)
             "Generate variable-rate data"
    , Option ['i'] ["integer"] (NoArg IntData)
             "Generate integer data"
    , Option ['r'] ["rate"] (ReqArg Rate "data-rate")
             "Set track rate"
    , Option ['l'] ["label"] (ReqArg Label "label")
             "Set track label"
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
        processOneOption config (Channels s) = do
            return $ config {channels = read s}
        processOneOption config (Watermark s) = do
            return $ config {wmLevel = read s}
        processOneOption config (Track s) = do
            return $ config {track = read s}

        processOneOption config Delta = do
            return $ config { spec = (spec config){specDeltaEncode = True} }
        processOneOption config ZLib = do
            return $ config { spec = (spec config){specZlibCompress = True} }
        processOneOption config Variable = do
            return $ config { variable = True
                            , spec = (spec config){specSRType = VariableSR}
                            }
        processOneOption config IntData = do
            return $ config { intData = True
                            , spec = setCodec (undefined :: Int) (spec config)
                            }
        processOneOption config (Rate s) = do
            return $ config { spec = (spec config){specRate = fromInteger $ read s} }
        processOneOption config (Label s) = do
            return $ config { spec = (spec config){specName = C.pack s} }

------------------------------------------------------------

zoomGen :: Command ()
zoomGen = defCmd {
          cmdName = "gen"
        , cmdHandler = zoomGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zoom", "foo.zoom")]
        }

zoomGenHandler :: App () ()
zoomGenHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ mapM_ (zoomWriteFile config) filenames

zoomWriteFile :: Config -> FilePath -> IO ()
zoomWriteFile Config{..} path
    | intData   = w ints
    | otherwise = w doubles
    where
    w :: (ZoomReadable a, ZoomWrite a, ZoomWritable a, ZoomWrite (SampleOffset, a))
      => [a] -> IO ()
    w d
        | variable && channels == 1 =
            writeData (sW >> mapM_ (write track) (zip (map SO [1,3..]) d))
        | channels == 1             =
            writeData (sW >> mapM_ (write track) d)
        | variable                  =
            writeData (sW >> mapM_ (write track)
                                   (zip (map SO [1,3..])
                                        (map (replicate channels) d)))
        | otherwise                 =
            writeData (sW >> mapM_ (write track) (map (replicate channels) d))

    writeData ds = do
        now <- getCurrentTime
        withFileWrite trackMap (Just now) (not noRaw) ds path

    sW = setWatermark track wmLevel
    trackMap = IM.singleton track spec'
    spec' | channels == 1 && intData = setCodec (undefined :: Int) spec
          | channels == 1            = setCodec (undefined :: Double) spec
          | intData                  = setCodecMultichannel channels (undefined :: Int) spec
          | otherwise                = setCodecMultichannel channels (undefined :: Double) spec

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
        , cmdExamples = [("Display info about foo.zoom", "foo.zoom")]
        }

zoomInfoHandler :: App () ()
zoomInfoHandler = mapM_ (liftIO . zoomInfoFile standardIdentifiers) =<< appArgs

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
    mapM_ (liftIO . zoomDumpFile standardIdentifiers (track config)) filenames

------------------------------------------------------------

zoomSummary :: Command ()
zoomSummary = defCmd {
          cmdName = "summary"
        , cmdHandler = zoomSummaryHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom-cache summary data"
        , cmdExamples = [("Read summary level 3 from foo.zoom", "3 foo.zoom")]
        }

zoomSummaryHandler :: App () ()
zoomSummaryHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO . (f (track config)) $ filenames
    where
        f trackNo (lvl:paths) = mapM_ (zoomDumpSummaryLevel (read lvl)
                                       standardIdentifiers trackNo) paths
        f _ _ = putStrLn "Usage: zoom-cache summary n file.zoom"

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
