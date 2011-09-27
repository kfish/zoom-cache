{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad.Trans (liftIO)
import Data.Default
import UI.Command

import Data.ZoomCache.Read
import Data.ZoomCache.Write

------------------------------------------------------------

zoomGen :: Command ()
zoomGen = defCmd {
          cmdName = "gen"
        , cmdHandler = zoomGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate zoom-cache data"
        , cmdExamples = [("Yo", "")]
        }

zoomGenHandler :: App () ()
zoomGenHandler = liftIO . zoomWriteFile =<< appArgs

zoomWriteFile :: [FilePath] -> IO ()
zoomWriteFile []       = return ()
zoomWriteFile (path:_) = do
    zoomWithFileW path $ do
        liftIO $ putStrLn path
        -- d <- liftIO zoomGenInt
        let d = zoomGenDouble
        mapM_ (uncurry (zoomPutDouble 1)) (zip [1..] d)

-- zoomGenInt :: IO [Int]
-- zoomGenInt = return [3, 3, 4, 3, 3, 6, 6, 7, 4, 9]

zoomGenDouble :: [Double]
zoomGenDouble = take 1000000 $ map ((* 1000.0) . sin) [0.0, 0.01 ..]

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
        , appCmds = [zoomGen, zoomDump, zoomSummary]
	}

longDesc :: String
longDesc = "Manipulate zoom-cache files"

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
