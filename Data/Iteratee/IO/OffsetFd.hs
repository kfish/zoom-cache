{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall #-}

module Data.Iteratee.IO.OffsetFd (
      enumFdRandomOBS
    , enumFileRandomOBS
    , fileDriverRandomFdOBS
    , fileDriverRandomOBS
) where

import Control.Arrow (second)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()
import Data.Offset (Offset(..))

import Control.Concurrent (yield)
import Control.Exception
import Control.Monad
import Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO (SeekMode(..))

import System.Posix hiding (FileOffset)

----------------------------------------------------------------------
-- Copied from Data.Iteratee.IO.Posix

import Foreign.C

myfdRead :: Fd -> Ptr CChar -> ByteCount -> IO (Either Errno ByteCount)
myfdRead (Fd fd) ptr n = do
  n' <- cRead fd ptr n
  if n' == -1 then liftM Left getErrno
     else return . Right . fromIntegral $ n'

foreign import ccall unsafe "unistd.h read" cRead
  :: CInt -> Ptr CChar -> CSize -> IO CInt

-- |The following fseek procedure throws no exceptions.
myfdSeek:: Fd -> SeekMode -> FileOffset -> IO (Either Errno FileOffset)
myfdSeek (Fd fd) mode off = do
  n' <- cLSeek fd off (mode2Int mode)
  if n' == -1 then liftM Left getErrno
     else return . Right  $ n'
 where mode2Int :: SeekMode -> CInt     -- From GHC source
       mode2Int AbsoluteSeek = 0
       mode2Int RelativeSeek = 1
       mode2Int SeekFromEnd  = 2

foreign import ccall unsafe "unistd.h lseek" cLSeek
  :: CInt -> FileOffset -> CInt -> IO FileOffset

----------------------------------------------------------------------
-- Copied from Data.Iteratee.IO

-- | The default buffer size.
defaultBufSize :: Int
defaultBufSize = 1024

----------------------------------------------------------------------

makefdCallback ::
  (MonadIO m) =>
  Ptr el
  -> ByteCount
  -> Fd
  -> st
  -> m (Either SomeException ((Bool, st), B.ByteString))
makefdCallback p bufsize fd st = do
  n <- liftIO $ myfdRead fd (castPtr p) bufsize
  case n of
    Left  _  -> return $ Left (error "myfdRead failed")
    Right 0  -> liftIO yield >> return (Right ((False, st), empty))
    Right n' -> liftM (\s -> Right ((True, st), s)) $
                  readFromPtr p (fromIntegral n')
    where
        readFromPtr buf l = liftIO $ B.packCStringLen (castPtr buf, l)

makefdCallbackOBS ::
  (MonadIO m) =>
  Ptr el
  -> ByteCount
  -> Fd
  -> st
  -> m (Either SomeException ((Bool, st), Offset ByteString))
makefdCallbackOBS p bufsize fd st = do
  o <- liftIO $ myfdSeek fd RelativeSeek 0
  case o of
      Left  _  -> return $ Left (error "myfdSeek failed")
      Right o' -> liftM (fmap (second (Offset o'))) (makefdCallback p bufsize fd st)

-- |A variant of enumFd that catches exceptions raised by the @Iteratee@.
enumFdCatchOBS
 :: forall e m a.(IException e, MonadCatchIO m)
    => Int
    -> Fd
    -> (e -> m (Maybe EnumException))
    -> Enumerator (Offset ByteString) m a
enumFdCatchOBS bs fd handler iter =
  let bufsize = bs
  in CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallbackCatch (makefdCallbackOBS p (fromIntegral bufsize) fd) handler () iter)

-- |The enumerator of a POSIX File Descriptor: a variation of @enumFd@ that
-- supports RandomIO (seek requests).
enumFdRandomOBS
 :: forall m a.(MonadCatchIO m) =>
    Int
    -> Fd
    -> Enumerator (Offset ByteString) m a
enumFdRandomOBS bs fd iter = enumFdCatchOBS bs fd handler iter
  where
    handler (SeekException off) =
      liftM (either
             (const . Just $ enStrExc "Error seeking within file descriptor")
             (const Nothing))
            . liftIO . myfdSeek fd AbsoluteSeek $ fromIntegral off

fileDriverOBS
  :: (MonadCatchIO m) =>
     (Int -> Fd -> Enumerator (Offset ByteString) m a)
     -> Int
     -> Iteratee (Offset ByteString) m a
     -> FilePath
     -> m a
fileDriverOBS enumf bufsize iter filepath = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (run <=< flip (enumf bufsize) iter)

-- |A version of fileDriverFd that supports seeking.
fileDriverRandomFdOBS
  :: (MonadCatchIO m) =>
     Int
     -> Iteratee (Offset ByteString) m a
     -> FilePath
     -> m a
fileDriverRandomFdOBS = fileDriverOBS enumFdRandomOBS

enumFile'OBS :: (MonadCatchIO m) =>
  (Int -> Fd -> Enumerator (Offset ByteString) m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> Enumerator (Offset ByteString) m a
enumFile'OBS enumf bufsize filepath iter = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (flip (enumf bufsize) iter)

enumFileRandomOBS ::
  (MonadCatchIO m)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator (Offset ByteString) m a
enumFileRandomOBS = enumFile'OBS enumFdRandomOBS

-- |Process a file using the given Iteratee.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandomOBS
  :: (MonadCatchIO m) =>
     Iteratee (Offset ByteString) m a
     -> FilePath
     -> m a
fileDriverRandomOBS = fileDriverRandomFdOBS defaultBufSize
