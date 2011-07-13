{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad.State
import Data.Bits
import Data.Default
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Word
import UI.Command
import Unsafe.Coerce (unsafeCoerce)

import Zoom.Write

------------------------------------------------------------

zoomGen :: Command ()
zoomGen = defCmd {
          cmdName = "gen"
        , cmdHandler = zoomGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate zoom data"
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
        d <- liftIO zoomGenDouble
        liftIO $ mapM_ (putStrLn . show) d
        mapM_ zoomPutDouble d

-- zoomGenInt :: IO [Int]
-- zoomGenInt = return [3, 3, 4, 3, 3, 6, 6, 7, 4, 9]

zoomGenDouble :: IO [Double]
zoomGenDouble = return [3.5, 3.5, 4.2, 3.7, 3.6, 6.3, 6.7, 7.7, 4.3, 9.3]

------------------------------------------------------------

zoomDump :: Command ()
zoomDump = defCmd {
          cmdName = "dump"
        , cmdHandler = zoomDumpHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom data"
        , cmdExamples = [("Yo", "")]
        }

zoomDumpHandler :: App () ()
zoomDumpHandler = liftIO . zoomReadFile =<< appArgs

zoomReadFile :: [FilePath] -> IO ()
zoomReadFile []       = return ()
zoomReadFile (path:_) = I.fileDriverRandom zReader path

zReader :: (Functor m, MonadIO m) => Iteratee [Word8] m ()
zReader = replicateM_ 10 (zReadFloat64be >>= liftIO . putStrLn . show)

-- zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
-- zReadInt32 = fromIntegral <$> I.endianRead4 I.LSB

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    c1 <- I.head
    c2 <- I.head
    c3 <- I.head
    c4 <- I.head
    c5 <- I.head
    c6 <- I.head
    c7 <- I.head
    c8 <- I.head
    let n :: Word64
        n = (((((((((((((fromIntegral c1
             `shiftL` 8) .|. fromIntegral c2)
             `shiftL` 8) .|. fromIntegral c3)
             `shiftL` 8) .|. fromIntegral c4)
             `shiftL` 8) .|. fromIntegral c5)
             `shiftL` 8) .|. fromIntegral c6)
             `shiftL` 8) .|. fromIntegral c7)
             `shiftL` 8) .|. fromIntegral c8
    return (unsafeCoerce n :: Double)

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

longDesc :: String
longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
