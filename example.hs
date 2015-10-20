--
-- Copyright © 2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

import Data.PhoneNumber
import Control.Monad
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = forever $
    S.getLine >>= print . flip parsePhoneNumber "AU"
