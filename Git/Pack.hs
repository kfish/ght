{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Git.Pack (
        -- * Types
	Pack(..),
        PackObject(..),
        PackObjectType,

	packPretty,

        -- * Iteratee
        packRead,
        packReadObject,

        -- * Paths
        packPath
) where

import Control.Applicative
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Iteratee as I
import Data.Iteratee.Binary
import Data.Iteratee.ZLib
import Data.Maybe (catMaybes)
import Data.Word
import System.FilePath
import System.Posix.Types

import Git.Path

------------------------------------------------------------

data Pack = Pack
    { packVersion :: Int
    , packNumObjects :: Int
    , packObjects :: [PackObject]
    } deriving (Show)

data PackObjectType = OBJ_COMMIT
                    | OBJ_TREE
                    | OBJ_BLOB
                    | OBJ_TAG
                    | OBJ_OFS_DELTA Int
                    | OBJ_REF_DELTA [Word8]
                    deriving (Show, Eq)

data PackObject = PackObject
    { poType :: PackObjectType
    , poSize :: Int
    , poData :: ByteString
    } deriving (Show)

------------------------------------------------------------

-- | Generate the pathname for a given packfile
packPath :: String -> IO FilePath
packPath pack = gitPath ("objects" </> "pack" </> ("pack-" ++ pack ++ ".pack"))

------------------------------------------------------------
-- packReader (Iteratee)
--

packRead :: FilePath -> IO (Maybe Pack)
packRead = I.fileDriverRandom packReader

packReader :: I.Iteratee ByteString IO (Maybe Pack)
packReader = do
    n <- I.heads "PACK"
    if (n == 4)
        then do
            ver <- fromIntegral <$> endianRead4 MSB
            num <- fromIntegral <$> endianRead4 MSB
            os <- catMaybes <$> sequence (replicate num packObjectRead)
            return $ Just (Pack ver num os)
        else return Nothing

packReadObject :: FilePath -> FileOffset -> IO (Maybe PackObject)
packReadObject fp off = I.fileDriverRandom (packReadObject' off) fp

packReadObject' :: FileOffset -> I.Iteratee ByteString IO (Maybe PackObject)
packReadObject' off = do
    n <- I.heads "PACK"
    if (n == 4)
        then do
            _ver <- fromIntegral <$> endianRead4 MSB
            _num <- fromIntegral <$> endianRead4 MSB
            I.seek off
            packObjectRead
        else return Nothing

packObjectRead :: I.Iteratee ByteString IO (Maybe PackObject)
packObjectRead = do
    x <- I.head
    let t = parseOBJ $ (x .&. 0x70) `shiftR` 4
        sz = castEnum (x .&. 0x0f)
    sz' <- if doNext x
               then readSize 4 sz
               else return sz
    t' <- readBase t
    d <- I.joinIM $ enumInflate Zlib defaultDecompressParams I.stream2stream
    return $ PackObject <$> t' <*> pure sz' <*> pure d
    where
        parseOBJ :: Word8 -> Maybe PackObjectType
        parseOBJ 1 = Just OBJ_COMMIT
        parseOBJ 2 = Just OBJ_TREE
        parseOBJ 3 = Just OBJ_BLOB
        parseOBJ 4 = Just OBJ_TAG
        parseOBJ 6 = Just (OBJ_OFS_DELTA 0)
        parseOBJ 7 = Just (OBJ_REF_DELTA [])
        parseOBJ _   = Nothing

        doNext :: Word8 -> Bool
        doNext x = (x .&. 0x80) /= 0

        readSize :: Int -> Int -> I.Iteratee ByteString IO Int
        readSize shft acc = do
            x <- I.head
            let sz = acc + (((castEnum (x .&. 0x7f)) :: Int) `shiftL` shft)
            if doNext x
                then readSize (shft+7) sz
                else return sz

        readBase :: Maybe PackObjectType
                 -> I.Iteratee ByteString IO (Maybe PackObjectType)
        readBase (Just (OBJ_OFS_DELTA 0))  =
            Just . OBJ_OFS_DELTA <$> readOFSBase 0 0
        readBase (Just (OBJ_REF_DELTA [])) =
            Just . OBJ_REF_DELTA <$> (sequence $ replicate 20 I.head)
        readBase (Just t)                  = return (Just t)
        readBase Nothing                   = return Nothing

        readOFSBase :: Int -> Int -> I.Iteratee ByteString IO Int
        readOFSBase shft acc = do
            x <- I.head
            let bs = acc + (((castEnum (x .&. 0x7f)) :: Int) `shiftL` shft)
            if doNext x
                then readOFSBase (shft+7) (bs+1)
                else return bs

        castEnum = toEnum . fromEnum


------------------------------------------------------------
-- packPretty
--

packPretty :: Pack -> L.ByteString
packPretty (Pack ver n _) =
	C.unlines [
		C.concat [(C.pack "Version:     "), C.pack (show ver)],
		C.concat [(C.pack "Num Objects: "), C.pack (show n)]
	]
