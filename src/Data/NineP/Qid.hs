{-# LANGUAGE NoImplicitPrelude #-}

-- 9P2000 messages are sent in little endian byte order rather than network byte order
-- (big endian)
module Data.NineP.Qid where

-- * Bin - a little endian encode/decode class for Binary
import           Control.Monad
import           Data.Serialize
import           Data.Word
import           qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BL
import           Protolude       hiding (get, put)
import qualified Test.QuickCheck as QC

import BitMask

type FileVersion = Word32 -- s for context including user state

-- <zhasha> bit 0 doesn't say anything about it being a file
-- <zhasha> it's a file if QTDIR isn't set
-- <zhasha> QTFILE is just a #define to make your intention clear  [10:50]
-- <joe9> " The name QTFILE, defined to be zero, identifies the value of the type for a plain file. " -- last line of http://man2.aiju.de/5/0intro  [10:51]
-- <zhasha> so when does zero become one?
-- <zhasha> it's not the zero'th bit. it's the number zero  [10:52]
-- <zhasha> as in qid.type == 0 means it's a regular plain file with no special anything
-- <joe9> ok, Thanks.  [10:53]
-- <zhasha> QTFILE exists so you can write in your program that qid->type = QTFILE instead of qid->type = 0, which is a tad more readable  [10:55]
-- <joe9> I thought the values were bit masks. such as having QTTMP | QTSYMLINK  [10:56]
-- <joe9> I did not realize that they were just values and not bit positions used to calculate the value.  [10:57]
-- <zhasha> They are for OR'ing together
-- <zhasha> but take a stab at what happens when you OR with 0
-- #define QTDIR		0x80		/* type bit for directories */
-- #define QTAPPEND	0x40		/* type bit for append only files */
-- #define QTEXCL		0x20		/* type bit for exclusive use files */
-- #define QTMOUNT		0x10		/* type bit for mounted channel */
-- #define QTAUTH		0x08		/* type bit for authentication file */
-- #define QTTMP		0x04		/* type bit for non-backed-up file */
-- #define QTSYMLINK	0x02		/* type bit for symbolic link */
-- #define QTFILE		0x00		/* type bits for plain file */
data QType
  = Unknown0 -- File
  | Unknown1 -- SymbolicLink not in 9P2000
  | NonBackedUp
  | Authentication
  | Unknown4 -- MountedChannel not specified in the manual
  | ExclusiveUse
  | AppendOnly
  | Directory
  deriving (Bounded, Enum, Eq, Show)

instance ToBitMask QType

instance QC.Arbitrary QType where
  arbitrary = QC.arbitraryBoundedEnum

-- | A Plan 9 Qid type.  See http://9p.cat-v.org for more information
data Qid = Qid
  { qType    :: ![QType] -- is a Word8 == uchar
  , qversion :: !FileVersion
  -- using Int instead of Word64 to avoid using fromIntegral all the time
  , qPath    :: !Int -- !Word64
  } deriving Show

instance Eq Qid where
  (==) (Qid t1 v1 p1) (Qid t2 v2 p2) =
    ((fromIntegral :: Word32 -> Word8) . toBitMask) t1 == ((fromIntegral :: Word32 -> Word8) . toBitMask) t2 && v1 == v2 && p1 == p2

instance Serialize Qid where
  get =
    fmap (Qid . fromBitMask . fromIntegral) getWord8 <*> getWord32le <*>
    fmap fromIntegral getWord64le
  put (Qid t v p) =
    (putWord8 . (fromIntegral :: Word32 -> Word8) . toBitMask) t >>
    putWord32le v >>
    putWord64le (fromIntegral p)

instance QC.Arbitrary Qid where
  arbitrary = do
    t <- QC.arbitrary
    v <- QC.arbitrarySizedBoundedIntegral
    p <- QC.arbitrarySizedBoundedIntegral
    return (Qid t v p)

-- http://stackoverflow.com/a/35604893
-- *Data.NineP.QType Protolude > showByteStringInHex ((runPut . putWord8 . fromIntegral . toBitMask) [File,Directory])
showByteStringInHex :: ByteString -> BL.ByteString
showByteStringInHex = BSB.toLazyByteString . BSB.byteStringHex
