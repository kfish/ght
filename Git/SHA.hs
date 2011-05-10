{-# OPTIONS -Wall #-}

module Git.SHA (
    showDigestBS,
    readDigestBS
) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.List (unfoldr)
import Numeric

------------------------------------------------------------
-- From Data.Digest.Pure.SHA

-- |Prints out a bytestring in hexadecimal. Just for convenience.
showDigestBS :: ByteString -> String
showDigestBS bs = foldr paddedShowHex [] (BS.unpack bs)
 where
   paddedShowHex x xs = intToDigit (fromIntegral (x `shiftR` 4))
                      : intToDigit (fromIntegral (x .&. 0xf))
                      : xs


------------------------------------------------------------
-- Read a string as a hex bytestring

readDigestBS :: String -> ByteString
readDigestBS = BS.pack . map (fst . head . readHex) . takeWhile (not . null) . unfoldr (Just . splitAt 2)
