--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE BangPatterns #-}

module Data.PhoneNumber
(
    -- * Types
    PhoneNumber(..),
    PhoneNumberParseError(..),

    -- * Parsing
    parsePhoneNumber,

    -- * Utility
    convertAlphaCharacters,
) where

import Data.PhoneNumber.LowLevel(PhoneNumber(..), PhoneNumberParseError(..))
import qualified Data.PhoneNumber.LowLevel as LowLevel
import Data.ByteString(ByteString, copy)
import System.IO.Unsafe(unsafePerformIO)

phoneUtilSingleton :: LowLevel.PhoneNumberUtil
phoneUtilSingleton = unsafePerformIO LowLevel.getPhoneNumberUtil
{-# NOINLINE phoneUtilSingleton #-}

-- | Try to parse a phone number
parsePhoneNumber
    :: ByteString
    -- ^ The phone number to parse
    -> ByteString
    -- ^ A two letter country code for the default region to be assumed
    -> Either PhoneNumberParseError PhoneNumber
parsePhoneNumber phone_no default_region = do
    let !phone_ref = unsafePerformIO LowLevel.newPhoneNumberRef
    let !r = unsafePerformIO $ LowLevel.parsePhoneNumber phoneUtilSingleton phone_ref phone_no default_region
    fmap (\_ -> unsafePerformIO $ LowLevel.copyPhoneNumberRef phone_ref) r

-- | Convert any alpha characters in a phone number to their equivalent keypad
-- numbers.
convertAlphaCharacters :: ByteString -> ByteString
convertAlphaCharacters number =
    let c = copy number
    in unsafePerformIO (c <$ LowLevel.unsafeConvertAlphaCharacters phoneUtilSingleton c)
