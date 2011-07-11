
module Main (
    main
) where

import Blaze.ByteString.Builder
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS
import Data.Default
import Data.Monoid
import System.IO
import UI.Command

------------------------------------------------------------

zoomGen = defCmd {
          cmdName = "gen"
        , cmdHandler = zoomGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate zoom data"
        , cmdExamples = [("Yo", "")]
        }

zoomGenHandler = liftIO . zoomWriteFile =<< appArgs

zoomWriteFile :: [FilePath] -> IO ()
zoomWriteFile []       = return ()
zoomWriteFile (path:_) = do
    h <- openFile path WriteMode
    putStrLn path
    d <- zoomGenInt
    mapM_ (putStrLn . show) d

    let b = mconcat $ map (fromInt32le . fromIntegral) d

    toByteStringIO (BS.hPut h) b
    hClose h

zoomGenInt :: IO [Int]
zoomGenInt = return [3, 3, 4, 3, 3, 6, 6, 7, 4, 9]

zoomGenDouble :: IO [Double]
zoomGenDouble = return [3.5, 3.5, 4.2, 3.7, 3.6, 6.3, 6.7, 7.7, 4.3, 9.3]

------------------------------------------------------------

zoomDump = defCmd {
          cmdName = "dump"
        , cmdHandler = zoomDumpHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom data"
        , cmdExamples = [("Yo", "")]
        }

zoomDumpHandler = liftIO . zoomReadFile =<< appArgs

zoomReadFile :: [FilePath] -> IO ()
zoomReadFile []       = return ()
zoomReadFile (path:_) = do
    h <- openFile path ReadMode
    putStrLn path
    bs <- BS.hGetContents h
    putStrLn . show $ bs
    hClose h

------------------------------------------------------------
-- The Application
--

zoom :: Application () ()
zoom = def {
          appName = "zoom"
        , appVersion = "0.1"
        , appAuthors = ["Conrad Parker"]
        , appBugEmail = "conrad@metadecks.org"
        , appShortDesc = "Trivial zoom inspection tools"
        , appLongDesc = longDesc
        , appCategories = ["Reading", "Writing"]
        , appSeeAlso = [""]
        , appProject = "Zoom"
        , appCmds = [zoomGen, zoomDump]
	}

longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
