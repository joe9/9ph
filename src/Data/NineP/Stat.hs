{-# LANGUAGE NoImplicitPrelude #-}

-- 9P2000 messages are sent in little endian byte order rather than network byte order
-- (big endian)
module Data.NineP.Stat where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word
import           Protolude       hiding (get, put)

import BitMask

import Data.NineP.QType (Qid)

data SType
  = OtherExecutePermission
  | OtherWritePermission
  | OtherReadPermission
  | GroupExecutePermission
  | GroupWritePermission
  | GroupReadPermission
  | UserExecutePermission
  | UserWritePermission
  | UserReadPermission
  | Temp
  | Authentication
  | ExclusiveUse
  | AppendOnly
  | Directory
  | Unknown
  deriving (Eq, Show)

instance Bounded SType where
  minBound = OtherExecutePermission
  maxBound = Directory

instance Enum SType where
  toEnum 0  = OtherExecutePermission
  toEnum 1  = OtherWritePermission
  toEnum 2  = OtherReadPermission
  toEnum 3  = GroupExecutePermission
  toEnum 4  = GroupWritePermission
  toEnum 5  = GroupReadPermission
  toEnum 6  = UserExecutePermission
  toEnum 7  = UserWritePermission
  toEnum 8  = UserReadPermission
  toEnum 26 = Temp
  toEnum 27 = Authentication
  toEnum 29 = ExclusiveUse
  toEnum 30 = AppendOnly
  toEnum 31 = Directory
  toEnum _  = Unknown
  fromEnum OtherExecutePermission = 0
  fromEnum OtherWritePermission   = 1
  fromEnum OtherReadPermission    = 2
  fromEnum GroupExecutePermission = 3
  fromEnum GroupWritePermission   = 4
  fromEnum GroupReadPermission    = 5
  fromEnum UserExecutePermission  = 6
  fromEnum UserWritePermission    = 7
  fromEnum UserReadPermission     = 8
  fromEnum Temp                   = 26
  fromEnum Authentication         = 27
  fromEnum ExclusiveUse           = 29
  fromEnum AppendOnly             = 30
  fromEnum Directory              = 31
  fromEnum Unknown                = 28

instance ToBitMask SType

-- | Provides information on a path entry at a 9P2000 server
data Stat = Stat
  { stTyp    :: !Word16
  , stDev    :: !Word32
  , stQid    :: !Qid
  , stMode   :: [SType]
  , stAtime  :: !Word32
  , stMtime  :: !Word32
  , stLength :: !Word64
  , stName   :: !ByteString
  , stUid    :: !ByteString
  , stGid    :: !ByteString
  , stMuid   :: !ByteString
  } deriving (Show, Eq)

instance Serialize Stat where
  get = do
    _ <- getWord16le -- size
    typ <- getWord16le
    dev <- getWord32le
    qid <- get
    mode <- fmap fromBitMask getWord32le
    atime <- getWord32le
    mtime <- getWord32le
    len <- getWord64le
    name <- getVariableByteString
    uid <- getVariableByteString
    gid <- getVariableByteString
    muid <- getVariableByteString
    return (Stat typ dev qid mode atime mtime len name uid gid muid)
  put (Stat typ dev qid mode atime mtime len name uid gid muid) =
    (putWord16le . fromIntegral) -- size
      (2 + -- typ
       4 + -- dev
       13 + -- qid
       4 + -- mode
       4 + -- atime
       4 + --  mtime
       8 + -- len
       BS.length name +
       BS.length uid +
       BS.length gid +
       BS.length muid) >>
    putWord16le typ >>
    putWord32le dev >>
    put qid >>
    putWord32le (toBitMask mode) >>
    putWord32le atime >>
    putWord32le mtime >>
    putWord64le len >>
    putVariableByteString name >>
    putVariableByteString uid >>
    putVariableByteString gid >>
    putVariableByteString muid

putVariableByteString :: ByteString -> PutM ()
putVariableByteString s =
  (putWord16le . fromIntegral . BS.length) s >> putByteString s

getVariableByteString :: Get ByteString
getVariableByteString = getWord16le >>= getByteString . fromIntegral >>= return
