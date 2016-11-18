{-# LANGUAGE NoImplicitPrelude #-}

-- 9P2000 messages are sent in little endian byte order rather than network byte order
-- (big endian)
module Data.NineP.OpenMode where

import           Control.Monad
import           Data.Bits
import           Data.Serialize
import           Protolude       hiding (get, put)
import qualified Test.QuickCheck as QC

-- from libc.h
-- #define	OREAD	0	/* open for read */
-- #define	OWRITE	1	/* write */
-- #define	ORDWR	2	/* read and write */
-- #define	OEXEC	3	/* execute, == read but check execute permission */
-- #define	OTRUNC	16	/* or'ed in (except for exec), truncate file first */
-- #define	OCEXEC	32	/* or'ed in, close on exec */
-- #define	ORCLOSE	64	/* or'ed in, remove on close */
-- #define	OEXCL	0x1000	/* or'ed in, exclusive use (create only) */
-- TODO ignoring the high nibble for now as I do not need that functionality
data OpenMode
  = Read
  | Write
  | ReadWrite
  | Execute
  deriving (Eq, Show, Bounded, Enum)

-- TODO ignoring the high nibble for now as I do not need that functionality
instance Serialize OpenMode where
  get = fmap (toEnum . fromIntegral . (.&.) 0x0f) getWord8
  put = putWord8 . (fromIntegral . fromEnum)

instance QC.Arbitrary OpenMode where
  arbitrary = QC.arbitraryBoundedEnum
