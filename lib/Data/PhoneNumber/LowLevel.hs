--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Low level phone number handling, more closely matches the underlying
-- libphonenumber API.
{-# LANGUAGE ViewPatterns #-}

module Data.PhoneNumber.LowLevel
(
    -- * Data types
    PhoneNumber(..),
    PhoneNumberRef,
    PhoneNumberParseError(..),
    PhoneNumberUtil(..),
    PhoneNumberType(..),
    PhoneNumberFormat(..),

    -- * References and parsing
    getPhoneNumberUtil,
    newPhoneNumberRef,
    parsePhoneNumber,

    -- * Utility
    unsafeConvertAlphaCharacters,

    -- * Extracting usable information
    copyPhoneNumberRef,

    getCountryCode,
    getNationalNumber,
    getExtension,
    getType,
    getFormatted,
    isPossibleNumber,
    isValidNumber,
) where

import           Control.Monad
import           Data.ByteString        (ByteString, useAsCStringLen)
import           Data.ByteString.Unsafe (unsafePackMallocCString,
                                         unsafeUseAsCStringLen)
import           Data.PhoneNumber.FFI
import           Data.Word
import           Foreign.ForeignPtr     (newForeignPtr, withForeignPtr)
import           Foreign.Ptr            (Ptr, nullPtr)

-- | A data type representation of a phone number, you can build one of these
-- with 'copyPhoneNumberRef' given a 'PhoneNumberRef', which is simply a
-- convenience for a series of calls to accessors.
data PhoneNumber =
    PhoneNumber {
        countryCode    :: Maybe Word64,
        nationalNumber :: Maybe Word64,
        extension      :: Maybe ByteString
    } deriving (Eq, Show)


-- | Grab the singleton PhoneNumberUtil class instance, needed to do useful
-- things with libphonenumber
getPhoneNumberUtil :: IO PhoneNumberUtil
getPhoneNumberUtil =
    PhoneNumberUtil <$> c_phone_number_util_get_instance

-- | Create a mutable reference to a PhoneNumber (C++ class) instance.
newPhoneNumberRef :: IO PhoneNumberRef
newPhoneNumberRef = do
    ptr <- c_phone_number_ctor
    if nullPtr == ptr
        then error "c_phone_number_ctor returned null ptr, out of memory?"
        else PhoneNumberRef <$> newForeignPtr c_phone_number_dtor ptr

-- | Parse a phone number.
parsePhoneNumber
    :: PhoneNumberUtil
    -- ^ The singleton PhoneNumberUtil reference
    -> PhoneNumberRef
    -- ^ The reference to be mutably updated
    -> ByteString
    -- ^ The bytestring to parse as a phone number
    -> ByteString
    -- ^ The default region to assume numbers are from, if ambiguous. e.g. "AU"
    -- for Australia
    -> IO (Either PhoneNumberParseError ())
parsePhoneNumber (PhoneNumberUtil util_ptr) (PhoneNumberRef f_ptr) number region =
    useAsCStringLen number $ \(number_str, fromIntegral -> number_len) ->
    useAsCStringLen region $ \(region_str, fromIntegral -> region_len) -> do
    retco <- withForeignPtr f_ptr $
        c_phone_number_util_parse util_ptr number_str number_len region_str region_len
    return $ case fromIntegral $ retco of
      0 -> Right ()
      err -> Left . toEnum $ err - 1

-- | Read the country code from a PhoneNumberRef
getCountryCode :: PhoneNumberRef -> IO (Maybe Word64)
getCountryCode =
    (fmap . fmap) fromIntegral . maybeFetch c_phone_number_has_country_code c_phone_number_get_country_code

-- | Read the national number (the phone number itself) from a PhoneNumberRef
getNationalNumber :: PhoneNumberRef -> IO (Maybe Word64)
getNationalNumber =
    (fmap . fmap) fromIntegral . maybeFetch c_phone_number_has_national_number c_phone_number_get_national_number

-- | Read the extension (e.g. 12345678x123) from a PhoneNumberRef
getExtension :: PhoneNumberRef -> IO (Maybe ByteString)
getExtension =
    maybeFetch c_phone_number_has_extension c_phone_number_get_extension >=> traverse unsafePackMallocCString

getType :: PhoneNumberUtil -> PhoneNumberRef -> IO PhoneNumberType
getType (PhoneNumberUtil util_ptr) (PhoneNumberRef ref_fptr) =
    withForeignPtr ref_fptr $ \ref_ptr ->
        toEnum . fromIntegral <$> c_phone_number_get_number_type util_ptr ref_ptr

getFormatted :: PhoneNumberUtil -> PhoneNumberRef -> PhoneNumberFormat -> IO ByteString
getFormatted (PhoneNumberUtil util_ptr) (PhoneNumberRef ref_fptr) format =
    withForeignPtr ref_fptr $ \ref_ptr ->
        c_phone_number_get_formatted util_ptr ref_ptr (fromIntegral $ fromEnum format) >>= unsafePackMallocCString


isPossibleNumber :: PhoneNumberUtil -> PhoneNumberRef -> IO Bool
isPossibleNumber (PhoneNumberUtil util_ptr) (PhoneNumberRef ref_fptr) =
    withForeignPtr ref_fptr $ \ref_ptr ->
        c_phone_number_is_possible_number util_ptr ref_ptr

isValidNumber :: PhoneNumberUtil -> PhoneNumberRef -> IO Bool
isValidNumber (PhoneNumberUtil util_ptr) (PhoneNumberRef ref_fptr) =
    withForeignPtr ref_fptr $ \ref_ptr ->
        c_phone_number_is_valid_number util_ptr ref_ptr

-- | Copy fields from a 'PhoneNumberRef' and create a 'PhoneNumber'
copyPhoneNumberRef :: PhoneNumberRef -> IO PhoneNumber
copyPhoneNumberRef ref =
    PhoneNumber <$> getCountryCode ref
                <*> getNationalNumber ref
                <*> getExtension ref

-- | Convert any alpha characters in a phone number to their equivalent keypad
-- numbers.
--
-- This modifies the ByteString in place.
unsafeConvertAlphaCharacters :: PhoneNumberUtil -> ByteString -> IO ()
unsafeConvertAlphaCharacters (PhoneNumberUtil util_ptr) number =
    unsafeUseAsCStringLen number $ \(number_str, fromIntegral -> number_len) ->
        c_phone_number_convert_alpha_characters_in_number util_ptr number_str number_len

-- * Helpers

maybeFetch
    :: (Ptr PhoneNumberRef -> IO Bool)
    -> (Ptr PhoneNumberRef -> IO a)
    -> PhoneNumberRef
    -> IO (Maybe a)
maybeFetch p f (PhoneNumberRef f_ptr) =
    withForeignPtr f_ptr $ \ptr -> do
        r <- p ptr
        if r
            then Just <$> f ptr
            else return Nothing
