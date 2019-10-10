{-# LANGUAGE ForeignFunctionInterface #-}

module Data.PhoneNumber.FFI (
    PhoneNumberRef(..),
    PhoneNumberUtil(..),
    PhoneNumberType(..),
    PhoneNumberFormat(..),
    PhoneNumberParseError(..),

    -- * Parsing and utility
    c_phone_number_ctor,
    c_phone_number_dtor,
    c_phone_number_util_get_instance,
    c_phone_number_util_parse,
    c_phone_number_convert_alpha_characters_in_number,

    -- * Accessors / metadata
    c_phone_number_has_country_code,
    c_phone_number_has_national_number,
    c_phone_number_has_extension,
    c_phone_number_get_country_code,
    c_phone_number_get_national_number,
    c_phone_number_get_extension,
    c_phone_number_get_number_type,
    c_phone_number_get_formatted,
    -- ** Validation
    c_phone_number_is_possible_number,
    c_phone_number_is_valid_number,
) where

import           Foreign.C.String   (CString)
import           Foreign.C.Types    (CInt (..), CULLong (..))
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.Ptr        (FunPtr, Ptr)

#include "c-phone-numbers.h"

-- | An opaque pointer to a PhoneNumberRef C++ class
data PhoneNumberRef = PhoneNumberRef { unPhoneNumberRef :: ForeignPtr PhoneNumberRef }
  deriving Show

-- | An opaque pointer to the (singleton) PhoneNumberUtil C++ class
data PhoneNumberUtil = PhoneNumberUtil { unPhoneNumberUtil :: Ptr PhoneNumberUtil }
  deriving Show

{# enum PhoneNumberType as PhoneNumberType {underscoreToCase} deriving (Eq, Show) #}

{# enum PhoneNumberFormat as PhoneNumberFormat {underscoreToCase} deriving (Eq, Show) #}

{# enum ErrorType as PhoneNumberParseError {underscoreToCase} omit (NO_PARSING_ERROR) deriving (Eq, Show) #}

--  | Create a PhoneNumber opaque pointer
foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_ctor"
    c_phone_number_ctor
        :: IO (Ptr PhoneNumberRef)

--  | Destroy a PhoneNumber
foreign import ccall unsafe "c-phone-numbers.h &_c_phone_number_dtor"
    c_phone_number_dtor
        :: FunPtr (Ptr PhoneNumberRef -> IO ())

--  | Get the singleton PhoneNumberUtil opaque pointer
foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_util_get_instance"
    c_phone_number_util_get_instance ::
        IO (Ptr PhoneNumberUtil)

-- | Call a C wrapper to parse a phone number with explicit length strings
foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_util_parse"
    c_phone_number_util_parse
        :: Ptr PhoneNumberUtil
        -> CString
        -> CInt
        -- ^ The phone number
        -> CString
        -> CInt
        -- ^ The region code (AU, US, etc)
        -> Ptr PhoneNumberRef
        -- ^ The pointer to write to
        -> IO CInt


foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_convert_alpha_characters_in_number"
    c_phone_number_convert_alpha_characters_in_number
        :: Ptr PhoneNumberUtil
        -> CString
        -> CInt
        -> IO ()

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_has_country_code"
    c_phone_number_has_country_code
        :: Ptr PhoneNumberRef
        -> IO Bool

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_has_national_number"
    c_phone_number_has_national_number
        :: Ptr PhoneNumberRef
        -> IO Bool

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_has_extension"
    c_phone_number_has_extension
        :: Ptr PhoneNumberRef
        -> IO Bool

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_get_country_code"
    c_phone_number_get_country_code
        :: Ptr PhoneNumberRef
        -> IO CULLong

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_get_national_number"
    c_phone_number_get_national_number
        :: Ptr PhoneNumberRef
        -> IO CULLong

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_get_extension"
    c_phone_number_get_extension
        :: Ptr PhoneNumberRef
        -> IO CString

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_util_get_number_type"
    c_phone_number_get_number_type
        :: Ptr PhoneNumberUtil
        -> Ptr PhoneNumberRef
        -> IO CInt

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_util_is_possible_number"
    c_phone_number_is_possible_number
        :: Ptr PhoneNumberUtil
        -> Ptr PhoneNumberRef
        -> IO Bool

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_util_is_valid_number"
    c_phone_number_is_valid_number
        :: Ptr PhoneNumberUtil
        -> Ptr PhoneNumberRef
        -> IO Bool

foreign import ccall unsafe "c-phone-numbers.h _c_phone_number_get_formatted"
    c_phone_number_get_formatted
        :: Ptr PhoneNumberUtil
        -> Ptr PhoneNumberRef
        -> CInt
        -> IO CString
