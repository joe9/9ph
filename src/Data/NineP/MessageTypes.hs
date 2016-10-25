{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Serialize
import Data.Word
import Protolude      hiding (get, put)

-- types in 9P2000 defined in <fcall.h>
-- | A variable message type that encapsulates the valid kinds of messages in a 9P2000 payload
-- | A type that enumerates all the valid /(and one invalid)/ message
-- expand --tabs=4 plan9port/include/fcall.h | sed --expression='1,/enum/d;/};/,$d' --expression="/{/d" --expression="s/= .*,/,/g" --expression="s/,/|/g"

newtype TransmitMessageType = MkTransmitMessageType {unTransmitMessageType :: Word8} deriving (Eq)

instance Serialize TransmitMessageType where
  get = fmap MkTransmitMessageType getWord8
  put = putWord8 . unTransmitMessageType

pattern Tversion :: TransmitMessageType
pattern Tversion = MkTransmitMessageType 0x64 -- 100
pattern Tauth :: TransmitMessageType
pattern Tauth = MkTransmitMessageType 0x66 -- 102
pattern Tattach :: TransmitMessageType
pattern Tattach = MkTransmitMessageType 0x68 -- 104
--  could reuse Terror to capture an invalid TransmitMessageType
-- pattern Terror :: TransmitMessageType
-- pattern Terror = MkTransmitMessageType 106 -- /* illegal */
pattern Tflush :: TransmitMessageType
pattern Tflush = MkTransmitMessageType 0x6c -- 108
pattern Twalk :: TransmitMessageType
pattern Twalk = MkTransmitMessageType 0x6e -- 110
pattern Topen :: TransmitMessageType
pattern Topen = MkTransmitMessageType 0x70 -- 112
pattern Tcreate :: TransmitMessageType
pattern Tcreate = MkTransmitMessageType 0x72 -- 114
pattern Tread :: TransmitMessageType
pattern Tread = MkTransmitMessageType 0x74 -- 116
pattern Twrite :: TransmitMessageType
pattern Twrite = MkTransmitMessageType 0x76 -- 118
pattern Tclunk :: TransmitMessageType
pattern Tclunk = MkTransmitMessageType 0x78 -- 120
pattern Tremove :: TransmitMessageType
pattern Tremove = MkTransmitMessageType 0x7a -- 122
pattern Tstat :: TransmitMessageType
pattern Tstat = MkTransmitMessageType 0x7c -- 124
pattern Twstat :: TransmitMessageType
pattern Twstat = MkTransmitMessageType 0x7e -- 126
-- pattern Topenfd :: TransmitMessageType
-- pattern Topenfd = MkTransmitMessageType 98

-- instance Show TransmitMessageType where
--   show = showTransmitMessageType

showTransmitMessageType :: TransmitMessageType -> Text
-- showTransmitMessageType Topenfd = "Topenfd"
showTransmitMessageType Tversion = "Tversion"
showTransmitMessageType Tauth = "Tauth"
showTransmitMessageType Tattach = "Tattach"
--  could reuse Terror to capture an invalid TransmitMessageType
-- showTransmitMessageType Terror = "Terror" -- /* illegal */
showTransmitMessageType Tflush = "Tflush"
showTransmitMessageType Twalk = "Twalk"
showTransmitMessageType Topen = "Topen"
showTransmitMessageType Tcreate = "Tcreate"
showTransmitMessageType Tread = "Tread"
showTransmitMessageType Twrite = "Twrite"
showTransmitMessageType Tclunk = "Tclunk"
showTransmitMessageType Tremove = "Tremove"
showTransmitMessageType Tstat = "Tstat"
showTransmitMessageType Twstat = "Twstat"
-- showTransmitMessageType Tmax = "Tmax"
showTransmitMessageType _ = "unknown"

newtype ResponseMessageType = MkResponseMessageType {unResponseMessageType :: Word8} deriving (Eq)

instance Serialize ResponseMessageType where
  get = fmap MkResponseMessageType getWord8
  put = putWord8 . unResponseMessageType

pattern Rversion :: ResponseMessageType
pattern Rversion = MkResponseMessageType 0x65 -- 101
pattern Rauth :: ResponseMessageType
pattern Rauth = MkResponseMessageType 0x67 -- 103
pattern Rattach :: ResponseMessageType
pattern Rattach = MkResponseMessageType 0x69 -- 105
pattern Rerror :: ResponseMessageType
pattern Rerror = MkResponseMessageType 0x6b -- 107
pattern Rflush :: ResponseMessageType
pattern Rflush = MkResponseMessageType 0x6d -- 109
pattern Rwalk :: ResponseMessageType
pattern Rwalk = MkResponseMessageType 0x6f -- 111
pattern Ropen :: ResponseMessageType
pattern Ropen = MkResponseMessageType 0x71 -- 113
pattern Rcreate :: ResponseMessageType
pattern Rcreate = MkResponseMessageType 0x73 -- 115
pattern Rread :: ResponseMessageType
pattern Rread = MkResponseMessageType 0x75 -- 117
pattern Rwrite :: ResponseMessageType
pattern Rwrite = MkResponseMessageType 0x77 -- 119
pattern Rclunk :: ResponseMessageType
pattern Rclunk = MkResponseMessageType 0x79 -- 121
pattern Rremove :: ResponseMessageType
pattern Rremove = MkResponseMessageType 0x7b -- 123
pattern Rstat :: ResponseMessageType
pattern Rstat = MkResponseMessageType 0x7d -- 125
pattern Rwstat :: ResponseMessageType
pattern Rwstat = MkResponseMessageType 0x7f -- 127
-- pattern Ropenfd :: ResponseMessageType
-- pattern Ropenfd = MkResponseMessageType 99

showResponseMessageType :: ResponseMessageType -> Text
-- showResponseMessageType Ropenfd = "Ropenfd"
showResponseMessageType Rversion = "Rversion"
showResponseMessageType Rauth = "Rauth"
showResponseMessageType Rattach = "Rattach"
showResponseMessageType Rerror = "Rerror"
showResponseMessageType Rflush = "Rflush"
showResponseMessageType Rwalk = "Rwalk"
showResponseMessageType Ropen = "Ropen"
showResponseMessageType Rcreate = "Rcreate"
showResponseMessageType Rread = "Rread"
showResponseMessageType Rwrite = "Rwrite"
showResponseMessageType Rclunk = "Rclunk"
showResponseMessageType Rremove = "Rremove"
showResponseMessageType Rstat = "Rstat"
showResponseMessageType Rwstat = "Rwstat"
showResponseMessageType _ = "unknown"
