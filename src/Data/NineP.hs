{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.NineP
-- Copyright   : Tim Newsham
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : David Leimbach <leimy2k@gmail.com>
-- Stability   : experimental
-- Portability : Only tested on GHC 6.12.1, uses TypeSynonymInstances
--
-- Module providing Binary serialization of 9P messages to and from lazy
-- ByteStrings.
--
-- This library does not currently provide any networking support or
-- wrappers for easy to write clients or servers, though that may come
-- with time as we decide the best way to implement these.
--
-- 9P2000 messages are sent in little endian byte order rather than network byte order
-- (big endian)
--
-- Lightly tested against an Inferno operating
-- system share with no authentication successfully.
-----------------------------------------------------------------------------
module Data.NineP where

-- * Bin - a little endian encode/decode class for Binary
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Serialize
import qualified Data.Serialize  as DS
import           Data.Word
import           Protolude       hiding (get, put)

import BitMask

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
  , qversion :: !Word32
  , qPath    :: !Word64
  } deriving (Show, Eq)

instance Serialize Qid where
  get =
    fmap (Qid . fromBitMask . fromIntegral) getWord8 <*> getWord32le <*>
    getWord64le
  put (Qid t v p) =
    (putWord8 . (fromIntegral :: Word32 -> Word8) . toBitMask) t >>
    putWord32le v >>
    putWord64le p

-- | Provides information on a path entry at a 9P2000 server
data Stat = Stat
  { stTyp    :: !Word16
  , stDev    :: !Word32
  , stQid    :: !Qid
  , stMode   :: !Word32
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
    typ <- getWord16le
    dev <- getWord32le
    qid <- get
    mode <- getWord32le
    atime <- getWord32le
    mtime <- getWord32le
    len <- getWord64le
    name <- getVariableByteString
    uid <- getVariableByteString
    gid <- getVariableByteString
    muid <- getVariableByteString
    return (Stat typ dev qid mode atime mtime len name uid gid muid)
  put (Stat typ dev qid mode atime mtime len name uid gid muid) =
    putWord16le typ >> putWord32le dev >> put qid >> putWord32le mode >>
    putWord32le atime >>
    putWord32le mtime >>
    putWord64le len >>
    putVariableByteString name >>
    putVariableByteString uid >>
    putVariableByteString gid >>
    putVariableByteString muid

data Rversion = Rversion
  { rvMaxMessageSize :: Word32
  , rvVersion        :: !ByteString
  }

-- size[4] Rversion tag[2] msize[4] version[s]
instance Serialize Rversion where
  get = do
    maxMessageSize <- getWord32le
    size <- getWord16le
    version <- getByteString (fromIntegral size)
    return (Rversion maxMessageSize version)
  put (Rversion ms v) = putWord32le ms >> putVariableByteString v

putVariableByteString :: ByteString -> PutM ()
putVariableByteString s =
  (putWord16le . fromIntegral . BS.length) s >> putByteString s

getVariableByteString :: Get ByteString
getVariableByteString = getWord16le >>= getByteString . fromIntegral >>= return

data Tversion = Tversion
  { tvMaxMesageSize :: !Word32
  , tvVersion       :: !ByteString
  }

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
  }

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
  }

instance Serialize Rattach where
  get = fmap Rattach DS.get
  put = DS.put . raQid

data Rerror = Rerror
  { reEname :: !ByteString
  }

-- size[4] Rerror tag[2] ename[s]
instance Serialize Rerror where
  get = fmap Rerror getVariableByteString
  put = putVariableByteString . reEname

data Tauth = Tauth
  { tauAfid  :: !Word32
  , tauUname :: !ByteString
  , tauAname :: !ByteString
  }

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
  }

instance Serialize Rauth where
  get = fmap Rauth DS.get
  put = DS.put . raAqid

data Tflush = Tflush
  { tfOldtag :: !Word16
  }

instance Serialize Tflush where
  get = fmap Tflush getWord16le
  put = putWord16le . tfOldtag

data Rflush =
  Rflush

data Twalk = Twalk
  { twFid    :: !Word32
  , twNewfid :: !Word32
  , twWnames :: ![ByteString]
  }

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
  }

--    size[4] Rwalk tag[2] nwqid[2] nwqid*(qid[13])
instance Serialize Rwalk where
  get = fmap Rwalk DS.get
  put (Rwalk qids) =
    (putWord16le . fromIntegral . length) qids >> mapM_ DS.put qids

data Topen = Topen
  { toFid  :: !Word32
  , toMode :: !Word8
  }

instance Serialize Topen where
  get = fmap Topen getWord32le <*> getWord8
  put (Topen f m) = putWord32le f >> putWord8 m

data Ropen = Ropen
  { roQid    :: !Qid
  , roIounit :: !Word32
  }

instance Serialize Ropen where
  get = fmap Ropen DS.get <*> getWord32le
  put (Ropen q m) = DS.put q >> putWord32le m

data Tcreate = Tcreate
  { tcrFid  :: !Word32
  , tcrName :: !ByteString
  , tcrPerm :: !Word32
  , tcrMode :: !Word8
  }

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
  }

instance Serialize Rcreate where
  get = fmap Rcreate DS.get <*> getWord32le
  put (Rcreate q m) = DS.put q >> putWord32le m

data Tread = Tread
  { trdFid    :: !Word32
  , trdOffset :: !Word64
  , trdCount  :: !Word32
  }

instance Serialize Tread where
  get = fmap Tread getWord32le <*> getWord64le <*> getWord32le
  put (Tread f o c) = putWord32le f >> putWord64le o >> putWord32le c

data Rread = Rread
  { rrdDat :: !ByteString
  }

--    size[4] Rread tag[2] count[4] data[count]
instance Serialize Rread where
  get = do
    dataCount <- getWord32le
    datas <- getByteString (fromIntegral dataCount)
    return (Rread datas)
  put (Rread d) = (putWord32le . fromIntegral . BS.length) d >> putByteString d

data Twrite = Twrite
  { twrFid    :: !Word32
  , twrOffset :: !Word64
  , twrDat    :: !ByteString
  }

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
  }

instance Serialize Rwrite where
  get = fmap Rwrite getWord32le
  put = putWord32le . rwCount

data Tclunk = Tclunk
  { tclFid :: !Word32
  }

instance Serialize Tclunk where
  get = fmap Tclunk getWord32le
  put = putWord32le . tclFid

data Rclunk =
  Rclunk

data Tremove = Tremove
  { trmFid :: !Word32
  }

instance Serialize Tremove where
  get = fmap Tremove getWord32le
  put = putWord32le . trmFid

data Rremove =
  Rremove

data Tstat = Tstat
  { tsFid :: !Word32
  }

instance Serialize Tstat where
  get = fmap Tstat getWord32le
  put = putWord32le . tsFid

data Rstat = Rstat
  { rsStat :: !Stat
  }

--    size[4] Rstat tag[2] stat[n]
instance Serialize Rstat where
  get = fmap Rstat get
  put = put . rsStat

data Twstat = Twstat
  { twsFid  :: !Word32
  , twsStat :: !Stat
  }

--    size[4] Rstat tag[2] stat[n]
instance Serialize Twstat where
  get = fmap Twstat getWord32le <*> get
  put (Twstat f s) = putWord32le f >> put s

data Rwstat =
  Rwstat
  deriving (Show, Eq)

-- | The message envelope type for all 9P2000 messages
data Msg = Msg
  { msgTyp  :: !Tag
  , msgTag  :: !Word16
  , msgBody :: ByteString
  } deriving (Show, Eq)

-- | A variable message type that encapsulates the valid kinds of messages in a 9P2000 payload
-- | A type that enumerates all the valid /(and one invalid)/ message types in 9P2000
data Tag
  = TTversion
  | TRversion
  | TTauth
  | TRauth
  | TTattach
  | TRattach
  | XXXTTerror
  | TRerror
  | TTflush
  | TRflush
  | TTwalk
  | TRwalk
  | TTopen
  | TRopen
  | TTcreate
  | TRcreate
  | TTread
  | TRread
  | TTwrite
  | TRwrite
  | TTclunk
  | TRclunk
  | TTremove
  | TRremove
  | TTstat
  | TRstat
  | TTwstat
  | TRwstat
  deriving (Show, Eq, Ord, Enum)

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
