#!/bin/sh

input="plan9port/include/libc.h"
# output="/tmp/keysymifdefs.txt"

cat <<EOH

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

EOH

echo ""
echo "#include \"${input}\""
echo ""

expand --tabs=4 plan9port/include/libc.h |
    sed --expression='1,/DIRMAX/d;/RFMEM/,$d' |
    sed --expression="s,^/,-- /," --expression="s/\(^#define \(\S*\) .*\)/-- \1\npattern \2 :: Constant\npattern \2 = MkConstant #{const \2} /"

