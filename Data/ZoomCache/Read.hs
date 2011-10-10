{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.Read (
    -- * Types
      Packet(..)

    -- * Functions
    , zoomDumpFile
    , zoomDumpSummary
    , zoomDumpSummaryLevel
    , zoomInfoFile
    , zoomReadFile
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Trans (lift, MonadIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Default
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Ratio
import Data.Word
import Text.Printf
import Unsafe.Coerce (unsafeCoerce)

import Data.ZoomCache.Common
import Data.ZoomCache.Summary

------------------------------------------------------------

data ZoomReader m = ZoomReader
    { zrReadFileInfo :: FileInfo -> m ()
    , zrTracks       :: IntMap (TrackReader m)
    , zrFileInfo     :: FileInfo
    }

data TrackReader m = TrackReader
    { _trTrack      :: TrackNo
    , trReadPacket  :: Packet -> m ()
    , trReadSummary :: Summary -> m ()
    }

data PacketData = PDDouble [Double] | PDInt [Int]

data Packet = Packet
    { packetTrack :: TrackNo
    , packetEntryTime :: TimeStamp
    , packetExitTime :: TimeStamp
    , packetCount :: Int
    , packetData :: PacketData
    , packetTimeStamps :: [TimeStamp]
    }

instance (Monad m) => Default (ZoomReader m) where
    def = ZoomReader (const (return ())) IM.empty def

------------------------------------------------------------

data FileInfo = FileInfo
    { fiGlobal :: Maybe Global
    , fiSpecs  :: IntMap TrackSpec
    }

instance Default FileInfo where
    def = FileInfo Nothing IM.empty

fiFull :: FileInfo -> Bool
fiFull (FileInfo (Just g) specs) = IM.size specs == noTracks g
fiFull _                         = False

------------------------------------------------------------

addTrack :: (Monad m) => TrackNo
         -> (Packet -> m ()) -> (Summary -> m ())
         -> ZoomReader m -> ZoomReader m
addTrack trackNo pFunc sFunc zr = zr { zrTracks =  (IM.insert trackNo tr (zrTracks zr)) }
    where
        tr = TrackReader trackNo pFunc sFunc

zoomInfoFile :: [FilePath] -> IO ()
zoomInfoFile = zoomReadFile def{zrReadFileInfo = info}

zoomDumpFile :: [FilePath] -> IO ()
zoomDumpFile = zoomReadFile (addTrack 1 dumpData (const (return ())) def)

zoomDumpSummary :: [FilePath] -> IO ()
zoomDumpSummary = zoomReadFile (addTrack 1 (const (return ())) dumpSummary def)

zoomDumpSummaryLevel :: Int -> [FilePath] -> IO ()
zoomDumpSummaryLevel lvl = zoomReadFile (addTrack 1 (const (return ())) (dumpSummaryLevel lvl) def)

zoomReadFile :: (Functor m, MonadCatchIO m)
             => ZoomReader m
             -> [FilePath]
             -> m ()
zoomReadFile _      []       = return ()
zoomReadFile reader (path:_) = I.fileDriverRandom (zReader reader) path

zReader :: (Functor m, MonadIO m)
        => ZoomReader m
        -> Iteratee [Word8] m ()
zReader zr = readHeaders zr >>= readPackets >> return ()

readHeaders :: (Functor m, MonadIO m)
            => ZoomReader m
            -> Iteratee [Word8] m (ZoomReader m)
readHeaders zr = do
    e <- I.isStreamFinished
    case e of
        Just _  -> return zr
        Nothing -> do
            zr' <- zReadHeader zr
            let fi = zrFileInfo zr'
            if (fiFull fi)
                then do
                    let fiFunc = zrReadFileInfo zr
                    lift $ fiFunc fi
                    return zr'
                else readHeaders zr'

readPackets :: (Functor m, MonadIO m)
            => ZoomReader m
            -> Iteratee [Word8] m (ZoomReader m)
readPackets zr = do
    e <- I.isStreamFinished
    case e of
        Just _  -> return zr
        Nothing -> do
            zr' <- zReadPacket zr
            readPackets zr'

info :: FileInfo -> IO ()
info FileInfo{..} = do
    maybe (return ()) (putStrLn . prettyGlobal) fiGlobal
    mapM_ (putStrLn . uncurry prettyTrackSpec) . IM.assocs $ fiSpecs

prettyGlobal :: Global -> String
prettyGlobal Global{..} = unlines
    [ "Version:\t\t" ++ show vMaj ++ "." ++ show vMin
    , "No. tracks:\t\t" ++ show noTracks
    , "Presentation-time:\t" ++ ratShow presentationTime
    , "Base-time:\t\t" ++ ratShow baseTime
    , "UTC baseTime:\t\t" ++ maybe "undefined" show baseUTC
    ]
    where
        Version vMaj vMin = version

prettyTrackSpec :: TrackNo -> TrackSpec -> String
prettyTrackSpec trackNo TrackSpec{..} = unlines
    [ "Track " ++ show trackNo ++ ":"
    , "\tName:\t" ++ LC.unpack specName
    , "\tType:\t" ++ show specType
    , "\tRate:\t" ++ show specDRType ++ " " ++ ratShow specRate
    ]

ratShow :: Rational -> String
ratShow r
    | d == 0 = "0"
    | d == 1 = show n
    | otherwise = show n ++ "/" ++ show d
    where
        n = numerator r
        d = denominator r

dumpData :: Packet -> IO ()
dumpData p = mapM_ (\(t,d) -> printf "%s: %s\n" t d) tds
    where
        tds = zip (map (show . unTS) $ packetTimeStamps p) vals
        vals = case packetData p of
            PDDouble ds -> map show ds
            PDInt is    -> map show is

dumpSummary :: Summary -> IO ()
dumpSummary SummaryDouble{..} = do
    putStrLn $ printf "[%d - %d] lvl: %d\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\tavg: %.3f\trms: %.3f"
        (unTS summaryEntryTime) (unTS summaryExitTime) summaryLevel
        summaryDoubleEntry summaryDoubleExit summaryDoubleMin summaryDoubleMax
        summaryAvg summaryRMS
dumpSummary SummaryInt{..} = do
    putStrLn $ printf "[%d - %d] lvl: %d\tentry: %d\texit: %df\tmin: %d\tmax: %d\tavg: %.3f\trms: %.3f"
        (unTS summaryEntryTime) (unTS summaryExitTime) summaryLevel
        summaryIntEntry summaryIntExit summaryIntMin summaryIntMax
        summaryAvg summaryRMS

dumpSummaryLevel :: Int -> Summary -> IO ()
dumpSummaryLevel level s
    | level == summaryLevel s = dumpSummary s
    | otherwise               = return ()

------------------------------------------------------------

parseHeader :: L.ByteString -> Maybe HeaderType
parseHeader h
    | h == globalHeader  = Just GlobalHeader
    | h == trackHeader   = Just TrackHeader
    | h == packetHeader  = Just PacketHeader
    | h == summaryHeader = Just SummaryHeader
    | otherwise              = Nothing

zReadPacket :: (Functor m, MonadIO m)
            => ZoomReader m
            -> Iteratee [Word8] m (ZoomReader m)
zReadPacket zr = do
    header <- I.joinI $ I.takeUpTo 8 I.stream2list
    case parseHeader (L.pack header) of
        Just PacketHeader -> do
            (trackNo, packet) <- readPacket zr
            case (packet, IM.lookup trackNo (zrTracks zr)) of
                (Just p, Just tr) -> lift $ (trReadPacket tr) p
                _                 -> return ()
            return zr
        Just SummaryHeader -> do
            (trackNo, summary) <- readSummary zr
            case (summary, IM.lookup trackNo (zrTracks zr)) of
                (Just s, Just tr) -> lift $ (trReadSummary tr) s
                _                 -> return ()
            return zr
        _ -> return zr

readPacket :: (Functor m, MonadIO m)
           => ZoomReader m
           -> Iteratee [Word8] m (TrackNo, Maybe Packet)
readPacket zr = do
    trackNo <- zReadInt32
    entryTime <- TS <$> zReadInt32
    exitTime <- TS <$> zReadInt32
    byteLength <- zReadInt32
    count <- zReadInt32
    packet <- case IM.lookup trackNo (fiSpecs . zrFileInfo $ zr) of
        Just TrackSpec{..} -> do
            d <- case specType of
                ZDouble -> do
                    PDDouble <$> replicateM count zReadFloat64be
                ZInt -> do
                    PDInt <$> replicateM count zReadInt32
            ts <- map TS <$> case specDRType of
                ConstantDR -> do
                    return $ take count [unTS entryTime ..]
                VariableDR -> do
                    replicateM count zReadInt32
            return $ Just (Packet trackNo entryTime exitTime count d ts)
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, packet)

readSummary :: (Functor m, MonadIO m)
            => ZoomReader m
            -> Iteratee [Word8] m (TrackNo, Maybe Summary)
readSummary zr = do
    trackNo <- zReadInt32
    lvl <- zReadInt32
    entryTime <- TS <$> zReadInt32
    exitTime <- TS <$> zReadInt32
    byteLength <- zReadInt32

    summary <- case IM.lookup trackNo (fiSpecs . zrFileInfo $ zr) of
        Just TrackSpec{..} -> do
            case specType of
                ZDouble -> do
                    let n = flip div 8 byteLength
                    [en,ex,mn,mx,avg,rms] <- replicateM n zReadFloat64be
                    return $ Just (SummaryDouble trackNo lvl entryTime exitTime
                                       en ex mn mx avg rms)
                ZInt -> do
                    [en,ex,mn,mx] <- replicateM 4 zReadInt32
                    [avg,rms] <- replicateM 2 zReadFloat64be
                    return $ Just (SummaryInt trackNo lvl entryTime exitTime
                                       en ex mn mx avg rms)
        Nothing -> do
            I.drop byteLength
            return Nothing
    return (trackNo, summary)

zReadHeader :: (Functor m, MonadIO m)
            => ZoomReader m
            -> Iteratee [Word8] m (ZoomReader m)
zReadHeader zr = do
    header <- I.joinI $ I.takeUpTo 8 I.stream2list
    case parseHeader (L.pack header) of
        Just TrackHeader -> do
            (trackNo, spec) <- readTrackHeader
            let fi = zrFileInfo zr
                fi' = fi{fiSpecs = IM.insert trackNo spec (fiSpecs fi)}
            return zr{ zrFileInfo = fi' }
        Just GlobalHeader -> do
            g <- readGlobalHeader
            let fi = zrFileInfo zr
                fi' = fi{fiGlobal = Just g}
            return zr{ zrFileInfo = fi' }
        _ -> return zr

readGlobalHeader :: (Functor m, MonadIO m) => Iteratee [Word8] m Global
readGlobalHeader = do
    v <- readVersion
    n <- zReadInt32
    p <- readRational64
    b <- readRational64
    _u <- L.pack <$> (I.joinI $ I.takeUpTo 20 I.stream2list)
    return $ Global v n p b Nothing

readVersion :: (Functor m, MonadIO m) => Iteratee [Word8] m Version
readVersion = do
    vMaj <- zReadInt16
    vMin <- zReadInt16
    return $ Version vMaj vMin

readTrackHeader :: (Functor m, MonadIO m) => Iteratee [Word8] m (TrackNo, TrackSpec)
readTrackHeader = do
    trackNo <- zReadInt32
    trackType <- readTrackType
    drType <- readDataRateType

    rate <- readRational64

    byteLength <- zReadInt32
    name <- L.pack <$> (I.joinI $ I.takeUpTo byteLength I.stream2list)

    let spec = TrackSpec trackType drType rate name

    return (trackNo, spec)

readTrackType :: (Functor m, MonadIO m) => Iteratee [Word8] m TrackType
readTrackType = do
    n <- zReadInt16
    case n of
        0 -> return ZDouble
        1 -> return ZInt
        _ -> error "Bad tracktype"

readDataRateType :: (Functor m, MonadIO m) => Iteratee [Word8] m DataRateType
readDataRateType = do
    n <- zReadInt16
    case n of
        0 -> return ConstantDR
        1 -> return VariableDR
        _ -> error "Bad data rate type"

----------------------------------------------------------------------

zReadInt16 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt16 = fromIntegral . u16_to_s16 <$> I.endianRead2 I.MSB
    where
        u16_to_s16 :: Word16 -> Int16
        u16_to_s16 = fromIntegral

zReadInt32 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt32 = fromIntegral . u32_to_s32 <$> I.endianRead4 I.MSB
    where
        u32_to_s32 :: Word32 -> Int32
        u32_to_s32 = fromIntegral

zReadInt64 :: (Functor m, MonadIO m) => Iteratee [Word8] m Int
zReadInt64 = fromIntegral . u64_to_s64 <$> I.endianRead8 I.MSB
    where
        u64_to_s64 :: Word64 -> Int64
        u64_to_s64 = fromIntegral

zReadFloat64be :: (Functor m, MonadIO m) => Iteratee [Word8] m Double
zReadFloat64be = do
    n <- I.endianRead8 I.MSB
    return (unsafeCoerce n :: Double)

readRational64 :: (Functor m, MonadIO m) => Iteratee [Word8] m Rational
readRational64 = do
    num <- zReadInt64
    den <- zReadInt64
    if (den == 0)
        then return 0
        else return $ (fromIntegral num) % (fromIntegral den)
