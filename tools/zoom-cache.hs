{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad.Trans (liftIO)
import Data.Default
import UI.Command

import Data.ZoomCache.Common
import Data.ZoomCache.Read
import Data.ZoomCache.Write

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
zoomGenHandler = liftIO . (zoomWriteFile ZoomDouble doubles) =<< appArgs

zoomWriteFile :: (ZoomPut a) => ZoomTrackType -> [a] -> [FilePath] -> IO ()
zoomWriteFile _     _ []       = return ()
zoomWriteFile ztype d (path:_) = withFileWrite (oneTrack ztype "gen")
    (mapM_ (uncurry (zPut 1)) (zip [1..] d)) path

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
zoomGenIHandler = liftIO . (zoomWriteFile ZoomInt ints) =<< appArgs

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
        , appCmds = [zoomGen, zoomGenI, zoomDump, zoomSummary]
	}

longDesc :: String
longDesc = "Manipulate zoom-cache files"

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
