module Git.Pack (
	Pack(..),
	packParse,
	packPretty
) where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Data.Word

data Pack = Pack {
	packVersion :: Word32,
	packNumObjects :: Word32
}

------------------------------------------------------------
-- packParse
--

packDeSerialize = do
	ver <- getWord32be 
	n <- getWord32be
	return (Pack ver n)

packParse bs = runGet packDeSerialize bs'
	where bs' = L.drop 4 bs

------------------------------------------------------------
-- packPretty
--

packPretty (Pack ver n) =
	C.unlines [
		C.concat [(C.pack "Version:     "), C.pack (show ver)],
		C.concat [(C.pack "Num Objects: "), C.pack (show n)]
	]
