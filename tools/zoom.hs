
module Main (
    main
) where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Default
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Monoid
import Data.Word
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
zoomReadFile (path:_) = zFile path

zFile = I.fileDriverRandom zReader

zReader :: (Functor m, MonadIO m) => Iteratee [Word8] m ()
zReader = replicateM_ 10 (zReadInt32 >>= liftIO . putStrLn . show)

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral <$> I.endianRead4 I.LSB

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
