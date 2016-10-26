{-# LANGUAGE NoImplicitPrelude #-}

-- 9P2000 messages are sent in little endian byte order rather than network byte order
-- (big endian)
module Data.NineP where

-- * Bin - a little endian encode/decode class for Binary
import           Control.Monad
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.Serialize
import qualified Data.Serialize          as DS
import           Data.String.Conversions
import           Data.Word
import           GHC.Show
import           Protolude               hiding (get, put)
import qualified Test.QuickCheck         as QC

import           Data.NineP.MessageTypes (ResponseMessageType,
                                          unResponseMessageType,
                                          TransmitMessageType, unTransmitMessageType)
import qualified Data.NineP.MessageTypes as MT
import           Data.NineP.Qid
import           Data.NineP.Stat

type Offset = Word64

type Count = Word32

type ResultingData = BS.ByteString

type WriteData = BS.ByteString

type Fid = Word32

type AFid = Word32

type NewFid = Word32

type Permissions = Word32

type UserName = ByteString

type AccessName = ByteString

type Tag = Word16

class ToNinePFormat a where
  toNinePFormat :: a -> Tag -> ByteString

data NineVersion
  = VerUnknown
  | Ver9P2000
  deriving (Eq, Enum, Bounded)

instance Show NineVersion where
  show = cs . showNineVersion

instance QC.Arbitrary NineVersion where
  arbitrary = QC.arbitraryBoundedEnum

showNineVersion :: NineVersion -> ByteString
showNineVersion Ver9P2000  = "9P2000"
showNineVersion VerUnknown = "unknown"

toNineVersion :: ByteString -> NineVersion
toNineVersion s =
  if BS.isPrefixOf "9P2000" s
    then Ver9P2000
    else VerUnknown

data Rversion = Rversion
  { rvMaxMessageSize :: !Word32
  , rvVersion        :: !NineVersion
  } deriving (Eq, Show)

-- size[4] Rversion tag[2] msize[4] version[s]
instance Serialize Rversion where
  get = do
    maxMessageSize <- getWord32le
    size <- getWord16le
    version <- (fmap toNineVersion . getByteString . fromIntegral) size
    return (Rversion maxMessageSize version)
  put (Rversion ms v) =
    putWord32le ms >> putVariableByteString (showNineVersion v)

instance ToNinePFormat Rversion where
  toNinePFormat = toNinePByteString MT.Rversion

-- http://stackoverflow.com/a/16440400
instance QC.Arbitrary Rversion where
  arbitrary = do
    s <- QC.arbitrary
    v <- QC.arbitrary
    return (Rversion s v)

toNinePByteString
  :: Serialize a
  => ResponseMessageType -> a -> Tag -> ByteString
toNinePByteString rt r tag =
  let message =
        runPut (putWord8 (unResponseMessageType rt) >> putWord16le tag >> put r)
  in runPut
       (putWord32le (fromIntegral (BS.length message + 4)) >>
        putByteString message)

toNinePNullDataByteString :: ResponseMessageType -> t -> Tag -> ByteString
toNinePNullDataByteString rt _ tag =
  runPut
    (putWord32le 7 >> putWord8 (unResponseMessageType rt) >> putWord16le tag)

toNinePByteStringT
  :: Serialize a
  => TransmitMessageType -> a -> Tag -> ByteString
toNinePByteStringT rt r tag =
  let message =
        runPut (putWord8 (unTransmitMessageType rt) >> putWord16le tag >> put r)
  in runPut
       (putWord32le (fromIntegral (BS.length message + 4)) >>
        putByteString message)

toNinePNullDataByteStringT :: TransmitMessageType -> t -> Tag -> ByteString
toNinePNullDataByteStringT rt _ tag =
  runPut
    (putWord32le 7 >> putWord8 (unTransmitMessageType rt) >> putWord16le tag)

data Tversion = Tversion
  { tvMaxMesageSize :: !Word32
  , tvVersion       :: !NineVersion
  } deriving (Eq, Show)

-- size[4] Tversion tag[2] msize[4] version[s]
instance Serialize Tversion where
  get = do
    maxMessageSize <- getWord32le
    size <- getWord16le
    version <- (fmap toNineVersion . getByteString . fromIntegral) size
    return (Tversion maxMessageSize version)
  put (Tversion s v) =
    putWord32le s >> putVariableByteString (showNineVersion v)

-- http://stackoverflow.com/a/16440400
instance QC.Arbitrary Tversion where
  arbitrary = Tversion <$> QC.arbitrary <*> QC.arbitrary

instance ToNinePFormat Tversion where
  toNinePFormat = toNinePByteStringT MT.Tversion

data Tattach = Tattach
  { taFid   :: !Word32
  , taAfid  :: !Word32
  , taUname :: !ByteString
  , taAname :: !ByteString
  } deriving (Eq, Show)

-- size[4] Tattach tag[2] fid[4] afid[4] uname[s] aname[s]
instance Serialize Tattach where
  get = do
    fid <- getWord32le
    afid <- getWord32le
    uname <- getVariableByteString
    aname <- getVariableByteString
    return (Tattach fid afid uname aname)
  put (Tattach f af u a) =
    putWord32le f >> putWord32le af >> putVariableByteString u >>
    putVariableByteString a

-- instance QC.Arbitrary ByteString where arbitrary = BS.pack <$> QC.arbitrary
instance QC.Arbitrary Tattach where
  arbitrary = do
    fid <- QC.arbitrary
    afid <- QC.arbitrary
    uname <- QC.arbitrary
    aname <- QC.arbitrary
    return (Tattach fid afid uname aname)

data Rattach = Rattach
  { raQid :: !Qid
  } deriving (Eq, Show)

instance Serialize Rattach where
  get = fmap Rattach DS.get
  put = DS.put . raQid

instance ToNinePFormat Rattach where
  toNinePFormat = toNinePByteString MT.Rattach

instance QC.Arbitrary Rattach where
  arbitrary = fmap Rattach QC.arbitrary

data Rerror = Rerror
  { reEname :: !ByteString
  } deriving (Eq, Show)

-- size[4] Rerror tag[2] ename[s]
instance Serialize Rerror where
  get = fmap Rerror getVariableByteString
  put = putVariableByteString . reEname

instance ToNinePFormat Rerror where
  toNinePFormat = toNinePByteString MT.Rerror

instance QC.Arbitrary Rerror where
  arbitrary = do
    ename <- QC.arbitrary
    return (Rerror ename)

data Tauth = Tauth
  { tauAfid  :: !Word32
  , tauUname :: !ByteString
  , tauAname :: !ByteString
  } deriving (Eq, Show)

-- size[4] Tauth tag[2] afid[4] uname[s] aname[s]
instance Serialize Tauth where
  get = do
    afid <- getWord32le
    uname <- getVariableByteString
    aname <- getVariableByteString
    return (Tauth afid uname aname)
  put (Tauth af u a) =
    putWord32le af >> putVariableByteString u >> putVariableByteString a

instance QC.Arbitrary Tauth where
  arbitrary = do
    auAfid <- QC.arbitrarySizedBoundedIntegral
    uname <- fmap BS.pack QC.arbitrary
    aname <- QC.arbitrary
    return (Tauth auAfid uname aname)

data Rauth = Rauth
  { raAqid :: !Qid
  } deriving (Eq, Show)

instance Serialize Rauth where
  get = fmap Rauth DS.get
  put = DS.put . raAqid

instance ToNinePFormat Rauth where
  toNinePFormat = toNinePByteString MT.Rauth

instance QC.Arbitrary Rauth where
  arbitrary = fmap Rauth QC.arbitrary

data Tflush = Tflush
  { tfOldtag :: !Word16
  } deriving (Eq, Show)

instance Serialize Tflush where
  get = fmap Tflush getWord16le
  put = putWord16le . tfOldtag

instance QC.Arbitrary Tflush where
  arbitrary = do
    ot <- QC.arbitrarySizedBoundedIntegral
    return (Tflush ot)

data Rflush =
  Rflush
  deriving (Eq, Show)

-- instance Serialize Rflush where
--   get = fmap Rflush Get
--   put Rflush = Put
instance ToNinePFormat Rflush where
  toNinePFormat = toNinePNullDataByteString MT.Rflush

data Twalk = Twalk
  { twFid    :: !Word32
  , twNewfid :: !Word32
  , twWnames :: ![ByteString]
  } deriving (Eq, Show)

--   size[4] Twalk tag[2] fid[4] newfid[4] nwname[2] nwname*(wname[s])
instance Serialize Twalk where
  get = do
    fid <- getWord32le
    newfid <- getWord32le
    numberOfWalkNames <- getWord16le
    anames <- replicateM (fromIntegral numberOfWalkNames) getVariableByteString
    return (Twalk fid newfid anames)
  put (Twalk f nf names) =
    putWord32le f >> putWord32le nf >>
    (putWord16le . fromIntegral . length) names >>
    mapM_ putVariableByteString names

-- instance QC.Arbitrary Twalk where
--   arbitrary = do
--     fid <- QC.arbitrarySizedBoundedIntegral
--     nfid <- QC.arbitrarySizedBoundedIntegral
--     names <- [BS.pack <$> QC.arbitrary]
--     return (Twalk fid nfid names)
data Rwalk = Rwalk
  { rwWqid :: ![Qid]
  } deriving (Eq, Show)

--    size[4] Rwalk tag[2] nwqid[2] nwqid*(qid[13])
instance Serialize Rwalk where
  get = do
    numberOfQids <- getWord16le
    qids <- replicateM (fromIntegral numberOfQids) DS.get
    return (Rwalk qids)
  put (Rwalk qids) =
    (putWord16le . fromIntegral . length) qids >> mapM_ DS.put qids

instance ToNinePFormat Rwalk where
  toNinePFormat = toNinePByteString MT.Rwalk

instance QC.Arbitrary Rwalk where
  arbitrary = fmap Rwalk QC.arbitrary

data OpenMode
  = Read
  | Write
  | ReadWrite
  | Executable
  deriving (Eq, Show, Enum, Bounded)

instance Serialize OpenMode where
  get = fmap (toEnum . fromIntegral) getWord8
  put = putWord8 . (fromIntegral . fromEnum)

instance QC.Arbitrary OpenMode where
  arbitrary = QC.arbitraryBoundedEnum

data Topen = Topen
  { toFid  :: !Word32
  , toMode :: !OpenMode
  } deriving (Eq, Show)

instance Serialize Topen where
  get = fmap Topen getWord32le <*> DS.get
  put (Topen f m) = putWord32le f >> DS.put m

instance QC.Arbitrary Topen where
  arbitrary = do
    fid <- QC.arbitrarySizedBoundedIntegral
    mode <- QC.arbitrary
    return (Topen fid mode)

data Ropen = Ropen
  { roQid    :: !Qid
  , roIounit :: !Word32
  } deriving (Eq, Show)

instance Serialize Ropen where
  get = fmap Ropen DS.get <*> getWord32le
  put (Ropen q m) = DS.put q >> putWord32le m

instance ToNinePFormat Ropen where
  toNinePFormat = toNinePByteString MT.Ropen

instance QC.Arbitrary Ropen where
  arbitrary = do
    fid <- QC.arbitrary
    mode <- QC.arbitrarySizedBoundedIntegral
    return (Ropen fid mode)

data Tcreate = Tcreate
  { tcrFid  :: !Word32
  , tcrName :: !ByteString
  , tcrPerm :: !Word32
  , tcrMode :: !OpenMode
  } deriving (Eq, Show)

-- size[4] Tcreate tag[2] fid[4] name[s] perm[4] mode[1]
instance Serialize Tcreate where
  get = do
    fid <- getWord32le
    name <- getVariableByteString
    perm <- getWord32le
    mode <- DS.get
    return (Tcreate fid name perm mode)
  put (Tcreate f n p m) =
    putWord32le f >> putVariableByteString n >> putWord32le p >> put m

instance QC.Arbitrary Tcreate where
  arbitrary = do
    fid <- QC.arbitrary
    name <- QC.arbitrary
    perm <- QC.arbitrarySizedBoundedIntegral
    mode <- QC.arbitrary
    return (Tcreate fid name perm mode)

data Rcreate = Rcreate
  { rcrQid    :: !Qid
  , rcrIounit :: !Word32
  } deriving (Eq, Show)

instance Serialize Rcreate where
  get = fmap Rcreate DS.get <*> getWord32le
  put (Rcreate q m) = DS.put q >> putWord32le m

instance ToNinePFormat Rcreate where
  toNinePFormat = toNinePByteString MT.Rcreate

instance QC.Arbitrary Rcreate where
  arbitrary = do
    qid <- QC.arbitrary
    iounit <- QC.arbitrarySizedBoundedIntegral
    return (Rcreate qid iounit)

data Tread = Tread
  { trdFid    :: !Word32
  , trdOffset :: !Word64
  , trdCount  :: !Word32
  } deriving (Eq, Show)

instance Serialize Tread where
  get = fmap Tread getWord32le <*> getWord64le <*> getWord32le
  put (Tread f o c) = putWord32le f >> putWord64le o >> putWord32le c

instance QC.Arbitrary Tread where
  arbitrary = do
    fid <- QC.arbitrarySizedBoundedIntegral
    offset <- QC.arbitrarySizedBoundedIntegral
    count <- QC.arbitrarySizedBoundedIntegral
    return (Tread fid offset count)

data Rread = Rread
  { rrdDat :: !ByteString
  } deriving (Eq, Show)

--    size[4] Rread tag[2] count[4] data[count]
instance Serialize Rread where
  get = do
    dataCount <- getWord32le
    datas <- getByteString (fromIntegral dataCount)
    return (Rread datas)
  put (Rread d) = (putWord32le . fromIntegral . BS.length) d >> putByteString d

instance ToNinePFormat Rread where
  toNinePFormat = toNinePByteString MT.Rread

instance QC.Arbitrary Rread where
  arbitrary = do
    dat <- QC.arbitrary
    return (Rread dat)

data Twrite = Twrite
  { twrFid    :: !Word32
  , twrOffset :: !Word64
  , twrDat    :: !ByteString
  } deriving (Eq, Show)

-- size[4] Twrite tag[2] fid[4] offset[8] count[4] data[count]
instance Serialize Twrite where
  get = do
    fid <- getWord32le
    offset <- getWord64le
    dataCount <- getWord32le
    datas <- getByteString (fromIntegral dataCount)
    return (Twrite fid offset datas)
  put (Twrite f o d) =
    putWord32le f >> putWord64le o >> (putWord32le . fromIntegral . BS.length) d >>
    putByteString d

instance QC.Arbitrary Twrite where
  arbitrary = do
    fid <- QC.arbitrarySizedBoundedIntegral
    offset <- QC.arbitrarySizedBoundedIntegral
    dat <- QC.arbitrary
    return (Twrite fid offset dat)

data Rwrite = Rwrite
  { rwCount :: !Word32
  } deriving (Eq, Show)

instance Serialize Rwrite where
  get = fmap Rwrite getWord32le
  put = putWord32le . rwCount

instance ToNinePFormat Rwrite where
  toNinePFormat = toNinePByteString MT.Rwrite

instance QC.Arbitrary Rwrite where
  arbitrary = do
    count <- QC.arbitrarySizedBoundedIntegral
    return (Rwrite count)

data Tclunk = Tclunk
  { tclFid :: !Word32
  } deriving (Eq, Show)

instance Serialize Tclunk where
  get = fmap Tclunk getWord32le
  put = putWord32le . tclFid

instance QC.Arbitrary Tclunk where
  arbitrary = fmap Tclunk QC.arbitrarySizedBoundedIntegral

data Rclunk =
  Rclunk
  deriving (Eq, Show)

instance ToNinePFormat Rclunk where
  toNinePFormat = toNinePNullDataByteString MT.Rclunk

data Tremove = Tremove
  { trmFid :: !Word32
  } deriving (Eq, Show)

instance Serialize Tremove where
  get = fmap Tremove getWord32le
  put = putWord32le . trmFid

instance QC.Arbitrary Tremove where
  arbitrary = fmap Tremove QC.arbitrarySizedBoundedIntegral

data Rremove =
  Rremove
  deriving (Eq, Show)

instance ToNinePFormat Rremove where
  toNinePFormat = toNinePNullDataByteString MT.Rremove

data Tstat = Tstat
  { tsFid :: !Word32
  } deriving (Eq, Show)

instance Serialize Tstat where
  get = fmap Tstat getWord32le
  put = putWord32le . tsFid

instance QC.Arbitrary Tstat where
  arbitrary = fmap Tstat QC.arbitrarySizedBoundedIntegral

data Rstat = Rstat
  { rsStat :: !Stat
  } deriving (Eq, Show)

-- The notation parameter[n] where n is not a constant represents a
-- variable-length parameter: n[2] followed by n bytes of data forming
-- the parameter.
--    size[4] Rstat tag[2] stat[n]
instance Serialize Rstat where
  get
  -- ignoring this size as it is not needed to parse Stat
   = do
    _ <- getWord16le
    stats <- get
    return (Rstat stats)
  put = putVariableByteString . runPut . put . rsStat

instance ToNinePFormat Rstat where
  toNinePFormat = toNinePByteString MT.Rstat

instance QC.Arbitrary Rstat where
  arbitrary = fmap Rstat QC.arbitrary

data Twstat = Twstat
  { twsFid  :: !Word32
  , twsStat :: !Stat
  } deriving (Eq, Show)

-- The notation parameter[n] where n is not a constant represents a
-- variable-length parameter: n[2] followed by n bytes of data forming
-- the parameter.
-- size[4] Twstat tag[2] fid[4] stat[n]
instance Serialize Twstat where
  get = do
    fid <- getWord32le
    _ <- getWord16le -- count or n bytes used by stat[n]
    stat <- get
    return (Twstat fid stat)
  put (Twstat f s) = putWord32le f >> (putVariableByteString . runPut . put) s

instance QC.Arbitrary Twstat where
  arbitrary = do
    fid <- QC.arbitrarySizedBoundedIntegral
    stat <- QC.arbitrary
    return (Twstat fid stat)

data Rwstat =
  Rwstat
  deriving (Show, Eq)

-- instance Serialize Rwstat where
--   get = pure Rwstat
--   put Rwstat = return Put
instance ToNinePFormat Rwstat where
  toNinePFormat = toNinePNullDataByteString MT.Rwstat
  --
-- getTag (Tversion  ) = TTversion
-- getTag (Rversion  ) = TRversion
-- getTag (Tauth   ) = TTauth
-- getTag (Rauth ) = TRauth
-- getTag (Tflush ) = TTflush
-- getTag (Rflush) = TRflush
-- getTag (Tattach    ) = TTattach
-- getTag (Rattach ) = TRattach
-- getTag (Rerror ) = TRerror
-- getTag (Twalk   ) = TTwalk
-- getTag (Rwalk ) = TRwalk
-- getTag (Topen  ) = TTopen
-- getTag (Ropen  ) = TRopen
-- getTag (Tcreate    ) = TTcreate
-- getTag (Rcreate  ) = TRcreate
-- getTag (Tread   ) = TTread
-- getTag (Rread ) = TRread
-- getTag (Twrite   ) = TTwrite
-- getTag (Rwrite ) = TRwrite
-- getTag (Tclunk ) = TTclunk
-- getTag (Rclunk) = TRclunk
-- getTag (Tremove ) = TTremove
-- getTag (Rremove) = TRremove
-- getTag (Tstat ) = TTstat
-- getTag (Rstat ) = TRstat
-- getTag (Twstat  ) = TTwstat
-- getTag (Rwstat) = TRwstat
-- -- | For every messages type, runs a Get parser to decode that type of payload from the 9P2000 stream
-- getVarMsg :: !Tag -> Get VarMsg
-- getVarMsg = undefined
-- getVarMsg TTversion = Tversion <$> get <*> get
-- -- getVarMsg TRversion = Rversion <$> get <*> get
-- getVarMsg TTauth = Tauth <$> get <*> get <*> get
-- getVarMsg TRauth = Rauth <$> get
-- getVarMsg XXXTTerror = error "there is no Terror"
-- getVarMsg TRerror = Rerror <$> get
-- getVarMsg TTflush = Tflush <$> get
-- getVarMsg TRflush = return Rflush
-- getVarMsg TTattach = Tattach <$> get <*> get <*> get <*> get
-- getVarMsg TRattach = Rattach <$> get
-- getVarMsg TTwalk = Twalk <$> get <*> get <*> getList16
-- getVarMsg TRwalk = Rwalk <$> getList16
-- getVarMsg TTopen = Topen <$> get <*> get
-- getVarMsg TRopen = Ropen <$> get <*> get
-- getVarMsg TTcreate = Tcreate <$> get <*> get <*> get <*> get
-- getVarMsg TRcreate = Rcreate <$> get <*> get
-- getVarMsg TTread = Tread <$> get <*> get <*> get
-- getVarMsg TRread = Rread <$> getBytes32
-- getVarMsg TTwrite = Twrite <$> get <*> get <*> getBytes32
-- getVarMsg TRwrite = Rwrite <$> get
-- getVarMsg TTclunk = Tclunk <$> get
-- getVarMsg TRclunk = return Rclunk
-- getVarMsg TTremove = Tremove <$> get
-- getVarMsg TRremove = return Rremove
-- getVarMsg TTstat = Tstat <$> get
-- getVarMsg TRstat = Rstat <$> getNestList16
-- getVarMsg TTwstat = Twstat <$> get <*> getNestList16
-- getVarMsg TRwstat = return Rwstat
-- -- | For every lower level VarMsg type, encodes a full wrapper around that type for use with 9P2000 streams
-- putVarMsg :: !VarMsg -> Put
-- putVarMsg = undefined
-- putVarMsg (Tversion a b) = put a >> put b
-- -- putVarMsg (Rversion a b) = put a >> put b
-- putVarMsg (Tauth a b c) = put a >> put b >> put c
-- putVarMsg (Rauth a) = put a
-- putVarMsg (Rerror a) = put a
-- putVarMsg (Tflush a) = put a
-- putVarMsg (Rflush) = return ()
-- putVarMsg (Tattach a b c d) = put a >> put b >> put c >> put d
-- putVarMsg (Rattach a) = put a
-- putVarMsg (Twalk a b c) = put a >> put b >> putList16 c
-- putVarMsg (Rwalk a) = putList16 a
-- putVarMsg (Topen a b) = put a >> put b
-- putVarMsg (Ropen a b) = put a >> put b
-- putVarMsg (Tcreate a b c d) = put a >> put b >> put c >> put d
-- putVarMsg (Rcreate a b) = put a >> put b
-- putVarMsg (Tread a b c) = put a >> put b >> put c
-- putVarMsg (Rread a) = putBytes32 a
-- putVarMsg (Twrite a b c) = put a >> put b >> putBytes32 c
-- putVarMsg (Rwrite a) = put a
-- putVarMsg (Tclunk a) = put a
-- putVarMsg (Rclunk) = return ()
-- putVarMsg (Tremove a) = put a
-- putVarMsg (Rremove) = return ()
-- putVarMsg (Tstat a) = put a
-- putVarMsg (Rstat a) = putNestList16 a
-- putVarMsg (Twstat a b) = put a >> putNestList16 b
-- putVarMsg (Rwstat) = return ()
--
-- --------------------------------------------------------------------
-- $example
--
-- Exchanging initial version data with any 9P2000 server
--
-- > module Main where
--
-- > import Data.Maybe
-- > import Control.Monad
-- > import qualified Data.ByteString.Lazy.Char8 as C
-- > import Network.Socket hiding (send, recv)
-- > import Network.Socket.ByteString.Lazy
-- > import Data.Int
-- > import Data.Binary.Get
-- > import Data.Binary.Put
-- > import Debug.Trace
-- > import Data.NineP
-- >
-- > connector :: !IO Socket
-- > connector = withSocketsDo $
-- >             do
-- >               ainfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6872")
-- >               let a = head ainfo
-- >               sock <- socket AFINET Stream defaultProtocol
--
-- At this point we've just created our socket to a machine on 127.0.0.1:6872
-- where we'd expect to see a 9P2000 server.
--
-- >               putStrLn "Trying to connect"
-- >               connect sock (addrAddress (traceShow a a))
-- >               putStrLn "connected!"
--
-- The socket is connected at this point, build up a TVersion message, asking
-- to speak to the server with the 9P2000 protocol.
--
-- The 1024 tells the server the maximum message size we'd like to support.
--
-- >               let version = Msg TTversion (-1) $ Tversion 1024 "9P2000"
-- >               putStrLn $ "About to send: " ++ show version
--
-- We now need to pack the message into a bytestring.  This is handled by the
-- Bin class instance /Msg/, and the serialization is handled by runPut.
-- We send this data to the socket.
--
-- >               send sock $ runPut (put version)
-- >               putStrLn "Getting response"
--
-- Now wait for a response from the server, evaluated runGet over it to
-- de-serialize it, and show it.
--
-- >               msg <- recv sock 50
-- >               let response = runGet get msg ::Msg
-- >               putStrLn $ show response
-- >               return sock
