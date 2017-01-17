{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Concurrent
import Control.Exception(evaluate)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Word
import Data.List
import Data.PhoneNumber

main :: IO ()
main = do
  threadSafety
  putStrLn "All tests passed"

threadSafety :: IO ()
threadSafety = do
  let phoneWds = [2164050000 .. (2164069999 :: Word64)]
      phones = map (B.pack . show) phoneWds
      n = length phones
  chan <- newChan
  forM_ phones $ \p -> void $ forkIO $ do
    pr <- either (error . show) return $ parsePhoneNumberRef p "US"
    nn <- evaluate $ refNationalNumber pr
    v <- evaluate $ refIsValidNumber pr
    writeChan chan $! (nn,v)

  out <- zip phoneWds . sort . take n <$> getChanContents chan
  forM_ out $ \ t@(wdExpected, (wdGot,valid))-> do
    unless (Just wdExpected == wdGot && valid) $
      error $ "threadSafety failed with: "++(show t)

  isEmpty <- isEmptyChan chan
  unless isEmpty $
    error "threadSafety expected chan to be empty!"

  putStrLn "OK"
