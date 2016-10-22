{-# LANGUAGE NoImplicitPrelude #-}

-- 9P2000 messages are sent in little endian byte order rather than network byte order
-- (big endian)
module Data.NineP where

-- * Bin - a little endian encode/decode class for Binary
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Serialize
import qualified Data.Serialize  as DS
import           Data.Word
import           Protolude       hiding (get, put)

import           Data.NineP.MessageTypes (ResponseMessageType,
                                          unResponseMessageType)
import qualified Data.NineP.MessageTypes as MT
import           Data.NineP.QType
import           Data.NineP.Stat

type Offset = Word64

type Count = Word32

type ResultingData = BS.ByteString

type WriteData = BS.ByteString

type Fid = Word32

type AFid = Word32

type NewFid = Word32

type Permissions = Word32

type Mode = Word8

type UserName = ByteString

type AccessName = ByteString

type Tag = Word16

class ToNinePFormat a where
  toNinePFormat :: a -> Tag -> ByteString

data Rversion = Rversion
  { rvMaxMessageSize :: !Word32
  , rvVersion        :: !ByteString
  } deriving (Eq, Show)

-- size[4] Rversion tag[2] msize[4] version[s]
instance Serialize Rversion where
  get = do
    maxMessageSize <- getWord32le
    size <- getWord16le
    version <- getByteString (fromIntegral size)
    return (Rversion maxMessageSize version)
  put (Rversion ms v) = putWord32le ms >> putVariableByteString v

instance ToNinePFormat Rversion where
  toNinePFormat = toNinePByteString MT.Rversion

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

data Tversion = Tversion
  { tvMaxMesageSize :: !Word32
  , tvVersion       :: !ByteString
  } deriving (Eq, Show)

-- size[4] Tversion tag[2] msize[4] version[s]
instance Serialize Tversion where
  get = do
    maxMessageSize <- getWord32le
    size <- getWord16le
    version <- getByteString (fromIntegral size)
    return (Tversion maxMessageSize version)
  put (Tversion s v) = putWord32le s >> putVariableByteString v

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

data Rattach = Rattach
  { raQid :: !Qid
  } deriving (Eq, Show)

instance Serialize Rattach where
  get = fmap Rattach DS.get
  put = DS.put . raQid

instance ToNinePFormat Rattach where
  toNinePFormat = toNinePByteString MT.Rattach

data Rerror = Rerror
  { reEname :: !ByteString
  } deriving (Eq, Show)

-- size[4] Rerror tag[2] ename[s]
instance Serialize Rerror where
  get = fmap Rerror getVariableByteString
  put = putVariableByteString . reEname

instance ToNinePFormat Rerror where
  toNinePFormat = toNinePByteString MT.Rerror

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

data Rauth = Rauth
  { raAqid :: !Qid
  } deriving (Eq, Show)

instance Serialize Rauth where
  get = fmap Rauth DS.get
  put = DS.put . raAqid

instance ToNinePFormat Rauth where
  toNinePFormat = toNinePByteString MT.Rauth

data Tflush = Tflush
  { tfOldtag :: !Word16
  } deriving (Eq, Show)

instance Serialize Tflush where
  get = fmap Tflush getWord16le
  put = putWord16le . tfOldtag

data Rflush =
  Rflush deriving (Eq, Show)

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

data Rwalk = Rwalk
  { rwWqid :: ![Qid]
  } deriving (Eq, Show)

--    size[4] Rwalk tag[2] nwqid[2] nwqid*(qid[13])
instance Serialize Rwalk where
  get = fmap Rwalk DS.get
  put (Rwalk qids) =
    (putWord16le . fromIntegral . length) qids >> mapM_ DS.put qids

instance ToNinePFormat Rwalk where
  toNinePFormat = toNinePByteString MT.Rwalk

data Topen = Topen
  { toFid  :: !Word32
  , toMode :: !Word8
  } deriving (Eq, Show)

instance Serialize Topen where
  get = fmap Topen getWord32le <*> getWord8
  put (Topen f m) = putWord32le f >> putWord8 m

data Ropen = Ropen
  { roQid    :: !Qid
  , roIounit :: !Word32
  } deriving (Eq, Show)

instance Serialize Ropen where
  get = fmap Ropen DS.get <*> getWord32le
  put (Ropen q m) = DS.put q >> putWord32le m

instance ToNinePFormat Ropen where
  toNinePFormat = toNinePByteString MT.Ropen

data Tcreate = Tcreate
  { tcrFid  :: !Word32
  , tcrName :: !ByteString
  , tcrPerm :: !Word32
  , tcrMode :: !Word8
  } deriving (Eq, Show)

-- size[4] Tcreate tag[2] fid[4] name[s] perm[4] mode[1]
instance Serialize Tcreate where
  get = do
    fid <- getWord32le
    name <- getVariableByteString
    perm <- getWord32le
    mode <- getWord8
    return (Tcreate fid name perm mode)
  put (Tcreate f n p m) =
    putWord32le f >> putVariableByteString n >> putWord32le p >> putWord8 m

data Rcreate = Rcreate
  { rcrQid    :: !Qid
  , rcrIounit :: !Word32
  } deriving (Eq, Show)

instance Serialize Rcreate where
  get = fmap Rcreate DS.get <*> getWord32le
  put (Rcreate q m) = DS.put q >> putWord32le m

instance ToNinePFormat Rcreate where
  toNinePFormat = toNinePByteString MT.Rcreate

data Tread = Tread
  { trdFid    :: !Word32
  , trdOffset :: !Word64
  , trdCount  :: !Word32
  } deriving (Eq, Show)

instance Serialize Tread where
  get = fmap Tread getWord32le <*> getWord64le <*> getWord32le
  put (Tread f o c) = putWord32le f >> putWord64le o >> putWord32le c

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

data Rwrite = Rwrite
  { rwCount :: !Word32
  } deriving (Eq, Show)

instance Serialize Rwrite where
  get = fmap Rwrite getWord32le
  put = putWord32le . rwCount

instance ToNinePFormat Rwrite where
  toNinePFormat = toNinePByteString MT.Rwrite

data Tclunk = Tclunk
  { tclFid :: !Word32
  } deriving (Eq, Show)

instance Serialize Tclunk where
  get = fmap Tclunk getWord32le
  put = putWord32le . tclFid

data Rclunk =
  Rclunk deriving (Eq, Show)

instance ToNinePFormat Rclunk where
  toNinePFormat = toNinePNullDataByteString MT.Rclunk

data Tremove = Tremove
  { trmFid :: !Word32
  } deriving (Eq, Show)

instance Serialize Tremove where
  get = fmap Tremove getWord32le
  put = putWord32le . trmFid

data Rremove =
  Rremove deriving (Eq, Show)

instance ToNinePFormat Rremove where
  toNinePFormat = toNinePNullDataByteString MT.Rremove

data Tstat = Tstat
  { tsFid :: !Word32
  } deriving (Eq, Show)

instance Serialize Tstat where
  get = fmap Tstat getWord32le
  put = putWord32le . tsFid

data Rstat = Rstat
  { rsStat :: !Stat
  } deriving (Eq, Show)

--    size[4] Rstat tag[2] stat[n]
instance Serialize Rstat where
  get = fmap Rstat get
  put = put . rsStat

instance ToNinePFormat Rstat where
  toNinePFormat = toNinePByteString MT.Rstat

data Twstat = Twstat
  { twsFid  :: !Word32
  , twsStat :: !Stat
  } deriving (Eq, Show)

--    size[4] Rstat tag[2] stat[n]
instance Serialize Twstat where
  get = fmap Twstat getWord32le <*> get
  put (Twstat f s) = putWord32le f >> put s

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
