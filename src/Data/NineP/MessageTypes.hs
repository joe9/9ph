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
module Data.NineP.MessageTypes where

import Data.Maybe
import Data.Bimap
import Data.Word
import Data.Serialize
import Protolude  hiding (get, put)

-- types in 9P2000 defined in <fcall.h>

-- | A variable message type that encapsulates the valid kinds of messages in a 9P2000 payload
-- | A type that enumerates all the valid /(and one invalid)/ message
-- expand --tabs=4 plan9port/include/fcall.h | sed --expression='1,/enum/d;/};/,$d' --expression="/{/d" --expression="s/= .*,/,/g" --expression="s/,/|/g"
data TransmitMessageType
  = Topenfd
  | Tversion
  | Tauth
  | Tattach
  --  reusing Terror to capture an invalid TransmitMessageType
  | Terror -- /* illegal */
  | Tflush
  | Twalk
  | Topen
  | Tcreate
  | Tread
  | Twrite
  | Tclunk
  | Tremove
  | Tstat
  | Twstat
--   | Tmax
  deriving (Show, Eq, Ord)

toTransmitMessageType :: Word8 -> TransmitMessageType
toTransmitMessageType = fromMaybe Terror . flip lookup transmitTypes

fromTransmitMessageType :: TransmitMessageType -> Word8
fromTransmitMessageType = fromMaybe 106 . flip lookupR transmitTypes

instance Serialize TransmitMessageType where
  get = fmap toTransmitMessageType getWord8
  put = putWord8 . fromTransmitMessageType

transmitTypes :: Bimap Word8 TransmitMessageType
transmitTypes =
  ( fromList . fmap swap)
  [ (Tversion, 100)
  , (Tauth, 102)
  , (Tattach, 104)
  --  reusing Terror to capture an invalid TransmitMessageType
  , (Terror, 106) -- /* illegal */
  , (Tflush, 108)
  , (Twalk, 110)
  , (Topen, 112)
  , (Tcreate, 114)
  , (Tread, 116)
  , (Twrite, 118)
  , (Tclunk, 120)
  , (Tremove, 122)
  , (Tstat, 124)
  , (Twstat, 126)
--   , (Tmax, 128)
  , (Topenfd, 98)
  ]

data ResponseMessageType
  =  Ropenfd
  | Rversion
  | Rauth
  | Rattach
  | Rerror
  | Rflush
  | Rwalk
  | Ropen
  | Rcreate
  | Rread
  | Rwrite
  | Rclunk
  | Rremove
  | Rstat
  | Rwstat
--   | Tmax
  deriving (Show, Eq, Ord)

responseTypes :: Bimap Word8 ResponseMessageType
responseTypes =
  ( fromList . fmap swap)
  [ (Rversion, 101)
  , (Rauth, 103)
  , (Rattach, 105)
  , (Rerror, 107)
  , (Rflush, 109)
  , (Rwalk, 111)
  , (Ropen, 113)
  , (Rcreate, 115)
  , (Rread, 117)
  , (Rwrite, 119)
  , (Rclunk, 121)
  , (Rremove, 123)
  , (Rstat, 125)
  , (Rwstat, 127)
  , (Ropenfd, 99)
  ]

instance Serialize ResponseMessageType where
  get = fmap toResponseMessageType getWord8
  put = putWord8 . fromResponseMessageType

toResponseMessageType :: Word8 -> ResponseMessageType
toResponseMessageType = fromMaybe Rerror . flip lookup responseTypes

fromResponseMessageType :: ResponseMessageType -> Word8
fromResponseMessageType = fromMaybe 107 . flip lookupR responseTypes
