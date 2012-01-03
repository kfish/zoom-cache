{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall #-}

module Data.Iteratee.IO.OffsetFd (
      enumFileRandom
    , fileDriverRandomFd
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()

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

makefdCallback ::
  (MonadIO m) =>
  Ptr el
  -> ByteCount
  -> Fd
  -> st
  -> m (Either SomeException ((Bool, st), ByteString))
makefdCallback p bufsize fd st = do
  n <- liftIO $ myfdRead fd (castPtr p) bufsize
  case n of
    Left  _  -> return $ Left (error "myfdRead failed")
    Right 0  -> liftIO yield >> return (Right ((False, st), empty))
    Right n' -> liftM (\s -> Right ((True, st), s)) $
                  readFromPtr p (fromIntegral n')
    where
        readFromPtr buf l = liftIO $ B.packCStringLen (castPtr buf, l)

-- |A variant of enumFd that catches exceptions raised by the @Iteratee@.
enumFdCatch
 :: forall e m a.(IException e, MonadCatchIO m)
    => Int
    -> Fd
    -> (e -> m (Maybe EnumException))
    -> Enumerator ByteString m a
enumFdCatch bs fd handler iter =
  let bufsize = bs
  in CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallbackCatch (makefdCallback p (fromIntegral bufsize) fd) handler () iter)

-- |The enumerator of a POSIX File Descriptor: a variation of @enumFd@ that
-- supports RandomIO (seek requests).
enumFdRandom
 :: forall m a.(MonadCatchIO m) =>
    Int
    -> Fd
    -> Enumerator ByteString m a
enumFdRandom bs fd iter = enumFdCatch bs fd handler iter
  where
    handler (SeekException off) =
      liftM (either
             (const . Just $ enStrExc "Error seeking within file descriptor")
             (const Nothing))
            . liftIO . myfdSeek fd AbsoluteSeek $ fromIntegral off

fileDriver
  :: (MonadCatchIO m) =>
     (Int -> Fd -> Enumerator ByteString m a)
     -> Int
     -> Iteratee ByteString m a
     -> FilePath
     -> m a
fileDriver enumf bufsize iter filepath = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (run <=< flip (enumf bufsize) iter)

-- |A version of fileDriverFd that supports seeking.
fileDriverRandomFd
  :: (MonadCatchIO m) =>
     Int
     -> Iteratee ByteString m a
     -> FilePath
     -> m a
fileDriverRandomFd = fileDriver enumFdRandom

enumFile' :: (MonadCatchIO m) =>
  (Int -> Fd -> Enumerator ByteString m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> Enumerator ByteString m a
enumFile' enumf bufsize filepath iter = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (flip (enumf bufsize) iter)

enumFileRandom ::
  (MonadCatchIO m)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator ByteString m a
enumFileRandom = enumFile' enumFdRandom

