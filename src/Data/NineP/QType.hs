{-# LANGUAGE NoImplicitPrelude #-}

-- 9P2000 messages are sent in little endian byte order rather than network byte order
-- (big endian)
module Data.NineP.QType where

-- * Bin - a little endian encode/decode class for Binary
import           Control.Monad
import           Data.Serialize
import           Data.Word
import           Protolude       hiding (get, put)

import BitMask

type FileVersion = Word32 -- s for context including user state

-- #define QTDIR		0x80		/* type bit for directories */
-- #define QTAPPEND	0x40		/* type bit for append only files */
-- #define QTEXCL		0x20		/* type bit for exclusive use files */
-- #define QTMOUNT		0x10		/* type bit for mounted channel */
-- #define QTAUTH		0x08		/* type bit for authentication file */
-- #define QTTMP		0x04		/* type bit for non-backed-up file */
-- #define QTSYMLINK	0x02		/* type bit for symbolic link */
-- #define QTFILE		0x00		/* type bits for plain file */
data QType
  = File
  | SymbolicLink
  | NonBackedUpFile
  | AuthenticationFile
  | MountedChannel
  | ExclusiveUseFile
  | AppendOnlyFile
  | Directory
  deriving (Bounded, Enum, Eq, Show)

instance ToBitMask QType

-- | A Plan 9 Qid type.  See http://9p.cat-v.org for more information
data Qid = Qid
  { qType    :: ![QType] -- is a Word8 == uchar
  , qversion :: !FileVersion
  -- using Int instead of Word64 to avoid using fromIntegral all the time
  , qPath    :: !Int -- !Word64
  } deriving (Show, Eq)

instance Serialize Qid where
  get =
    fmap (Qid . fromBitMask . fromIntegral) getWord8 <*> getWord32le <*>
    fmap fromIntegral getWord64le
  put (Qid t v p) =
    (putWord8 . (fromIntegral :: Word32 -> Word8) . toBitMask) t >>
    putWord32le v >>
    putWord64le (fromIntegral p)
