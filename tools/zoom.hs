{-# LANGUAGE RecordWildCards #-}

module Main (
    main
) where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Control.Monad.State
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Default
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Monoid
import Data.Word
import System.IO
import UI.Command
import Unsafe.Coerce (unsafeCoerce)

------------------------------------------------------------

(<>) = mappend

data ZoomState = ZoomState
    { zoomHandle  :: Handle
    , zoomBuilder :: Builder
    }

zoomOpenW :: FilePath -> IO ZoomState
zoomOpenW path = do
    h <- openFile path WriteMode
    return (ZoomState h mempty)

zoomPutInt :: Int -> ZoomState -> ZoomState
zoomPutInt d z = z { zoomBuilder = zoomBuilder z <> (fromInt32le . fromIntegral) d }

zoomPutDouble :: Double -> ZoomState -> ZoomState
zoomPutDouble d z = z { zoomBuilder = zoomBuilder z <> (fromWord64be . toWord64) d }

zoomFlush :: ZoomState -> IO ZoomState
zoomFlush z@ZoomState{..} = do
    toByteStringIO (BS.hPut zoomHandle) zoomBuilder
    return z { zoomBuilder = mempty }
    
zoomClose :: ZoomState -> IO ()
zoomClose z@ZoomState{..} = do
    _ <- zoomFlush z
    hClose zoomHandle

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
    z <- zoomOpenW path
    putStrLn path
    -- d <- zoomGenInt
    d <- zoomGenDouble
    mapM_ (putStrLn . show) d
    let z' = foldl (flip zoomPutDouble) z d
    zoomFlush z'
    zoomClose z'

toWord64 :: Double -> Word64
toWord64 = unsafeCoerce

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
zReader = replicateM_ 10 (zReadFloat64be >>= liftIO . putStrLn . show)

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral <$> I.endianRead4 I.LSB

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

longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
