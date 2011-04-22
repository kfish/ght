module Git.Pack (
        -- * Pack object
	Pack(..),
	packPretty,

        -- * ByteString / Binary
	packParse,

        -- * Iteratee
        packRead,

        -- * Paths
        packPath
) where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import qualified Data.Iteratee as I
import Data.Iteratee.Binary
import Data.Word
import System.FilePath

import Git.Path

------------------------------------------------------------

data Pack = Pack {
	packVersion :: Word32,
	packNumObjects :: Word32
} deriving (Show)

------------------------------------------------------------

-- | Generate the pathname for a given packfile
packPath pack = gitPath ("objects" </> "pack" </> ("pack-" ++ pack ++ ".pack"))

------------------------------------------------------------
-- packParse (ByteString / Binary)
--

packDeSerialize = do
	ver <- getWord32be 
	n <- getWord32be
	return (Pack ver n)

packParse bs = runGet packDeSerialize bs'
	where bs' = L.drop 4 bs

------------------------------------------------------------
-- packReader (Iteratee)
--

packRead :: FilePath -> IO (Maybe Pack)
packRead = I.fileDriverRandom packReader

packReader :: I.Iteratee [Word8] IO (Maybe Pack)
packReader = do
    n <- I.heads (toWord8s "PACK")
    if (n == 4)
        then do
            ver <- endianRead4 MSB
            n <- endianRead4 MSB
            return $ Just (Pack ver n)
        else return Nothing
    where
        toWord8s = map (toEnum . fromEnum)

------------------------------------------------------------
-- packPretty
--

packPretty (Pack ver n) =
	C.unlines [
		C.concat [(C.pack "Version:     "), C.pack (show ver)],
		C.concat [(C.pack "Num Objects: "), C.pack (show n)]
	]
