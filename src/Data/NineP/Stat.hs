{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import qualified Test.QuickCheck as QC

import BitMask

import Data.NineP.Qid hiding (Directory)

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
  | Unknown9
  | Unknown10
  | Unknown11
  | Unknown12
  | Unknown13
  | Unknown14
  | Unknown15
  | Unknown16
  | Unknown17
  | Unknown18
  | Unknown19
  | Unknown20
  | Unknown21
  | Unknown22
  | Unknown23
  | Unknown24
  | Unknown25
  | Temp
  | Authentication
  | Unknown28
  | ExclusiveUse
  | AppendOnly
  | Directory
  deriving (Eq, Show, Bounded, Enum)

instance ToBitMask SType

-- for quickcheck
instance QC.Arbitrary SType where
  arbitrary = QC.arbitraryBoundedEnum

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
  } deriving (Show)

instance Eq Stat where
  (Stat t1 d1 q1 m1 a1 mt1 l1 n1 u1 g1 mu1) == (Stat t2 d2 q2 m2 a2 mt2 l2 n2 u2 g2 mu2) =
    t1 == t2 &&
    d1 == d2 &&
    q1 == q2 &&
    toBitMask m1 == toBitMask m2 &&
    a1 == a2 &&
    mt1 == mt2 && l1 == l2 && n1 == n2 && u1 == u2 && g1 == g2 && mu1 == mu2

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
       2 + -- storing the length of name
       BS.length name +
       2 + -- storing the length of uid
       BS.length uid +
       2 + -- storing the length of gid
       BS.length gid +
       2 + -- storing the length of muid
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

instance QC.Arbitrary ByteString where
  arbitrary = BS.pack <$> QC.arbitrary

instance QC.Arbitrary Stat where
  arbitrary = do
    t <- QC.arbitrarySizedBoundedIntegral
    d <- QC.arbitrarySizedBoundedIntegral
    q <- QC.arbitrary
    m <- QC.arbitrary
    a <- QC.arbitrarySizedBoundedIntegral
    mt <- QC.arbitrarySizedBoundedIntegral
    l <- QC.arbitrarySizedBoundedIntegral
    name <- QC.arbitrary
    uid <- QC.arbitrary
    gid <- QC.arbitrary
    muid <- QC.arbitrary
    return (Stat t d q m a mt l name uid gid muid)
--     return (Stat 0 0 q [Directory] 0 0 0 "" "" "" "")
