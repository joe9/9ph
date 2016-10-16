
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Bindings where

import Foreign

newtype Constant = MkConstant { unConstant :: Word32 } deriving (Eq, Show)

-- https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms

-- generated using the below commands:
-- sh generate_bindings.sh >|Bindings.hsc


#include "plan9port/include/libc.h"

-- #define ERRMAX  128 /* max length of error string */
pattern ERRMAX :: Constant
pattern ERRMAX = MkConstant #{const ERRMAX} 

-- #define MORDER  0x0003  /* mask for bits defining order of mounting */
pattern MORDER :: Constant
pattern MORDER = MkConstant #{const MORDER} 
-- #define MREPL   0x0000  /* mount replaces object */
pattern MREPL :: Constant
pattern MREPL = MkConstant #{const MREPL} 
-- #define MBEFORE 0x0001  /* mount goes before others in union directory */
pattern MBEFORE :: Constant
pattern MBEFORE = MkConstant #{const MBEFORE} 
-- #define MAFTER  0x0002  /* mount goes after others in union directory */
pattern MAFTER :: Constant
pattern MAFTER = MkConstant #{const MAFTER} 
-- #define MCREATE 0x0004  /* permit creation in mounted directory */
pattern MCREATE :: Constant
pattern MCREATE = MkConstant #{const MCREATE} 
-- #define MCACHE  0x0010  /* cache some data */
pattern MCACHE :: Constant
pattern MCACHE = MkConstant #{const MCACHE} 
-- #define MMASK   0x0017  /* all bits on */
pattern MMASK :: Constant
pattern MMASK = MkConstant #{const MMASK} 

-- #define OREAD   0   /* open for read */
pattern OREAD :: Constant
pattern OREAD = MkConstant #{const OREAD} 
-- #define OWRITE  1   /* write */
pattern OWRITE :: Constant
pattern OWRITE = MkConstant #{const OWRITE} 
-- #define ORDWR   2   /* read and write */
pattern ORDWR :: Constant
pattern ORDWR = MkConstant #{const ORDWR} 
-- #define OEXEC   3   /* execute, == read but check execute permission */
pattern OEXEC :: Constant
pattern OEXEC = MkConstant #{const OEXEC} 
-- #define OTRUNC  16  /* or'ed in (except for exec), truncate file first */
pattern OTRUNC :: Constant
pattern OTRUNC = MkConstant #{const OTRUNC} 
-- #define OCEXEC  32  /* or'ed in, close on exec */
pattern OCEXEC :: Constant
pattern OCEXEC = MkConstant #{const OCEXEC} 
-- #define ORCLOSE 64  /* or'ed in, remove on close */
pattern ORCLOSE :: Constant
pattern ORCLOSE = MkConstant #{const ORCLOSE} 
-- #define ODIRECT 128 /* or'ed in, direct access */
pattern ODIRECT :: Constant
pattern ODIRECT = MkConstant #{const ODIRECT} 
-- #define ONONBLOCK 256   /* or'ed in, non-blocking call */
pattern ONONBLOCK :: Constant
pattern ONONBLOCK = MkConstant #{const ONONBLOCK} 
-- #define OEXCL   0x1000  /* or'ed in, exclusive use (create only) */
pattern OEXCL :: Constant
pattern OEXCL = MkConstant #{const OEXCL} 
-- #define OLOCK   0x2000  /* or'ed in, lock after opening */
pattern OLOCK :: Constant
pattern OLOCK = MkConstant #{const OLOCK} 
-- #define OAPPEND 0x4000  /* or'ed in, append only */
pattern OAPPEND :: Constant
pattern OAPPEND = MkConstant #{const OAPPEND} 

-- #define AEXIST  0   /* accessible: exists */
pattern AEXIST :: Constant
pattern AEXIST = MkConstant #{const AEXIST} 
-- #define AEXEC   1   /* execute access */
pattern AEXEC :: Constant
pattern AEXEC = MkConstant #{const AEXEC} 
-- #define AWRITE  2   /* write access */
pattern AWRITE :: Constant
pattern AWRITE = MkConstant #{const AWRITE} 
-- #define AREAD   4   /* read access */
pattern AREAD :: Constant
pattern AREAD = MkConstant #{const AREAD} 

-- /* Segattch */
-- #define SG_RONLY    0040    /* read only */
pattern SG_RONLY :: Constant
pattern SG_RONLY = MkConstant #{const SG_RONLY} 
-- #define SG_CEXEC    0100    /* detach on exec */
pattern SG_CEXEC :: Constant
pattern SG_CEXEC = MkConstant #{const SG_CEXEC} 

-- #define NCONT   0   /* continue after note */
pattern NCONT :: Constant
pattern NCONT = MkConstant #{const NCONT} 
-- #define NDFLT   1   /* terminate after note */
pattern NDFLT :: Constant
pattern NDFLT = MkConstant #{const NDFLT} 
-- #define NSAVE   2   /* clear note but hold state */
pattern NSAVE :: Constant
pattern NSAVE = MkConstant #{const NSAVE} 
-- #define NRSTR   3   /* restore saved state */
pattern NRSTR :: Constant
pattern NRSTR = MkConstant #{const NRSTR} 

-- /* bits in Qid.type */
-- #define QTDIR       0x80        /* type bit for directories */
pattern QTDIR :: Constant
pattern QTDIR = MkConstant #{const QTDIR} 
-- #define QTAPPEND    0x40        /* type bit for append only files */
pattern QTAPPEND :: Constant
pattern QTAPPEND = MkConstant #{const QTAPPEND} 
-- #define QTEXCL      0x20        /* type bit for exclusive use files */
pattern QTEXCL :: Constant
pattern QTEXCL = MkConstant #{const QTEXCL} 
-- #define QTMOUNT     0x10        /* type bit for mounted channel */
pattern QTMOUNT :: Constant
pattern QTMOUNT = MkConstant #{const QTMOUNT} 
-- #define QTAUTH      0x08        /* type bit for authentication file */
pattern QTAUTH :: Constant
pattern QTAUTH = MkConstant #{const QTAUTH} 
-- #define QTTMP       0x04        /* type bit for non-backed-up file */
pattern QTTMP :: Constant
pattern QTTMP = MkConstant #{const QTTMP} 
-- #define QTSYMLINK   0x02        /* type bit for symbolic link */
pattern QTSYMLINK :: Constant
pattern QTSYMLINK = MkConstant #{const QTSYMLINK} 
-- #define QTFILE      0x00        /* type bits for plain file */
pattern QTFILE :: Constant
pattern QTFILE = MkConstant #{const QTFILE} 

-- /* bits in Dir.mode */
-- #define DMDIR       0x80000000  /* mode bit for directories */
pattern DMDIR :: Constant
pattern DMDIR = MkConstant #{const DMDIR} 
-- #define DMAPPEND    0x40000000  /* mode bit for append only files */
pattern DMAPPEND :: Constant
pattern DMAPPEND = MkConstant #{const DMAPPEND} 
-- #define DMEXCL      0x20000000  /* mode bit for exclusive use files */
pattern DMEXCL :: Constant
pattern DMEXCL = MkConstant #{const DMEXCL} 
-- #define DMMOUNT     0x10000000  /* mode bit for mounted channel */
pattern DMMOUNT :: Constant
pattern DMMOUNT = MkConstant #{const DMMOUNT} 
-- #define DMAUTH      0x08000000  /* mode bit for authentication file */
pattern DMAUTH :: Constant
pattern DMAUTH = MkConstant #{const DMAUTH} 
-- #define DMTMP       0x04000000  /* mode bit for non-backed-up file */
pattern DMTMP :: Constant
pattern DMTMP = MkConstant #{const DMTMP} 
-- #define DMSYMLINK   0x02000000  /* mode bit for symbolic link (Unix, 9P2000.u) */
pattern DMSYMLINK :: Constant
pattern DMSYMLINK = MkConstant #{const DMSYMLINK} 
-- #define DMDEVICE    0x00800000  /* mode bit for device file (Unix, 9P2000.u) */
pattern DMDEVICE :: Constant
pattern DMDEVICE = MkConstant #{const DMDEVICE} 
-- #define DMNAMEDPIPE 0x00200000  /* mode bit for named pipe (Unix, 9P2000.u) */
pattern DMNAMEDPIPE :: Constant
pattern DMNAMEDPIPE = MkConstant #{const DMNAMEDPIPE} 
-- #define DMSOCKET    0x00100000  /* mode bit for socket (Unix, 9P2000.u) */
pattern DMSOCKET :: Constant
pattern DMSOCKET = MkConstant #{const DMSOCKET} 
-- #define DMSETUID    0x00080000  /* mode bit for setuid (Unix, 9P2000.u) */
pattern DMSETUID :: Constant
pattern DMSETUID = MkConstant #{const DMSETUID} 
-- #define DMSETGID    0x00040000  /* mode bit for setgid (Unix, 9P2000.u) */
pattern DMSETGID :: Constant
pattern DMSETGID = MkConstant #{const DMSETGID} 

-- #define DMREAD      0x4     /* mode bit for read permission */
pattern DMREAD :: Constant
pattern DMREAD = MkConstant #{const DMREAD} 
-- #define DMWRITE     0x2     /* mode bit for write permission */
pattern DMWRITE :: Constant
pattern DMWRITE = MkConstant #{const DMWRITE} 
-- #define DMEXEC      0x1     /* mode bit for execute permission */
pattern DMEXEC :: Constant
pattern DMEXEC = MkConstant #{const DMEXEC} 

