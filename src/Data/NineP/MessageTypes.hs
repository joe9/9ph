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

-- * Bin - a little endian encode/decode class for Binary
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Serialize
import qualified Data.Serialize  as DS
import           Data.Word
import           Protolude       hiding (get, put)

import BitMask

-- | A variable message type that encapsulates the valid kinds of messages in a 9P2000 payload
-- | A type that enumerates all the valid /(and one invalid)/ message
  -- types in 9P2000 defined in <fcall.h>
data MessageType =
          Tversion |
        Rversion |
        Tauth |
        Rauth |
        Tattach |
        Rattach |
        Terror |
        Rerror |
        Tflush |
        Rflush |
        Twalk |
        Rwalk |
        Topen |
        Ropen |
        Tcreate |
        Rcreate |
        Tread |
        Rread |
        Twrite |
        Rwrite |
        Tclunk |
        Rclunk |
        Tremove |
        Rremove |
        Tstat |
        Rstat |
        Twstat |
        Rwstat |
        Tmax |

        Topenfd |
        Ropenfd

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
