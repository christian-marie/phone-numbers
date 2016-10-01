phone-numbers - Parse phone numbers with Google's libphonenumber
=========================================================

[![Travis Status](http://travis-ci.org/christian-marie/phone-numbers.png?branch=master)](https://travis-ci.org/christian-marie/phone-numbers)


```haskell
--
-- Copyright © 2016 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

import Data.PhoneNumber
import Control.Monad
import Data.Monoid
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = forever $ do
    l <- S.getLine

    S.putStrLn "Number:"
    print $ parsePhoneNumber l "AU"
    print (refType <$> parsePhoneNumberRef l "AU")
    S.putStrLn $ "Characters keypad normalised: " <> convertAlphaCharacters l
```
