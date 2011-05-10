{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Git.PackIndex (
        dumpRawPackIndex,

        -- * Paths
        idxPath
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Foreign.Ptr
import Foreign.Storable
import Data.Storable.Endian
import System.FilePath
import System.IO.MMap
import System.Posix.Types
import Text.Printf

import Git.SHA
import Git.Path

------------------------------------------------------------

data IDX = IDX1 {
      idx1Size       :: Int
    , idx1Fanout     :: Ptr (BigEndian Word32)
    , idx1Offsets    :: Ptr (BigEndian Word32)
    } | IDX2 {
      idx2Size       :: Int
    , idx2Fanout     :: Ptr (BigEndian Word32)
    , idx2SHA1s      :: Ptr (BigEndian Word32)
    , idx2CRCs       :: Ptr (BigEndian Word32)
    , idx2Offsets    :: Ptr (BigEndian Word32)
    , idx264bOffsets :: Ptr (BigEndian Word32)
    -- , idx2PackCSum   :: Ptr (BigEndian Word32)
    -- , idx2IdxCSum    :: Ptr (BigEndian Word32)
    }

------------------------------------------------------------
-- | Public API

-- | Number of objects in the corresponding .pack file
idxSize   :: IDX -> Int
idxSize IDX1{..} = idx1Size
idxSize IDX2{..} = idx2Size

-- | Nth SHA1
idxSha1   :: IDX -> Int -> IO BS.ByteString
idxSha1 IDX1{..} n
    | n >= idx1Size = outOfRange
    | otherwise     = do
        let cs = idx1Offsets `plusPtr` (4 + (n * 24))
        BS.packCStringLen (cs, 20)
idxSha1 IDX2{..} n
    | n >= idx2Size = outOfRange
    | otherwise     = do
        let cs = idx2SHA1s `plusPtr` (n * 20)
        BS.packCStringLen (cs, 20)

-- | Nth CRC
idxCRC    :: IDX -> Int -> IO (Maybe Word32)
idxCRC IDX1{..} n
    | n >= idx1Size = outOfRange
    | otherwise     = return Nothing
idxCRC IDX2{..} n
    | n >= idx2Size = outOfRange
    | otherwise     = do
        BE crc <- peekElemOff idx2CRCs n
        return (Just crc)

-- | Nth offset
idxOffset :: IDX -> Int -> IO FileOffset
idxOffset IDX1{..} n
    | n >= idx1Size = outOfRange
    | otherwise     = do
        BE off <- peekByteOff idx1Offsets (n * 24)
        return . fromIntegral $ (off :: Word32)
idxOffset IDX2{..} n
    | n >= idx2Size = outOfRange
    | otherwise     = do
        BE off <- peekElemOff idx2Offsets n
        return . fromIntegral $ off

outOfRange :: IO a
outOfRange = error "Index out of range"

------------------------------------------------------------
-- Debugging

dumpIdx :: IDX -> IO ()
dumpIdx idx@IDX1{..} = do
    putStrLn "IDX Version 1"
    dumpIdx' idx
dumpIdx idx@IDX2{..} = do
    putStrLn "IDX Version 2"
    dumpIdx' idx

dumpIdx' :: IDX -> IO ()
dumpIdx' idx = do
    putStrLn $ show (idxSize idx) ++ " objects"
    mapM_ f [0..(idxSize idx)-1]
    where
        f i = do
            o <- fromIntegral <$> idxOffset idx i
            let o' = printf "0x%04x" (o :: Int)
            s <- idxSha1 idx i
            c <- maybe "" ((" CRC: " ++) . show) <$> idxCRC idx i
            putStrLn $ show i ++ ": " ++ o' ++ " SHA: " ++ showDigestBS s ++ c

------------------------------------------------------------

-- | Generate the pathname for a given packfile
idxPath :: String -> IO FilePath
idxPath idx = gitPath ("objects" </> "pack" </> ("pack-" ++ idx ++ ".idx"))

idxHeader :: Word32
idxHeader = 0xff744f63

dumpRawPackIndex :: FilePath -> IO String
dumpRawPackIndex fp = do
    (ptr, rawsize, offset, size) <- mmapFilePtr fp ReadOnly Nothing
    let start :: Ptr (BigEndian Word32)
        start = ptr `plusPtr` offset
    BE hdr <- peek start
    idx <- if (hdr == idxHeader)
        then do
            BE ver <- peekElemOff start 1
            case ver of
                2 -> mkIDX2 start size
                _ -> error "Unknown version"
        else mkIDX1 start size
    dumpIdx idx
    return $ "Mapped region offset " ++ (show offset) ++ " size " ++ (show size)

mkIDX1 :: Ptr (BigEndian Word32) -> Int -> IO IDX
mkIDX1 start size = do
    let fanout = start
    BE n <- peekElemOff fanout 255
    let n' = fromIntegral (n :: Word32)
    let offsets = fanout `plusPtr` (256 * 4)
    return (IDX1 n' fanout offsets)

mkIDX2 :: Ptr (BigEndian Word32) -> Int -> IO IDX
mkIDX2 start size = do
    let fanout = start `plusPtr` (2 * 4)
    BE n <- peekElemOff fanout 255
    let n' = fromIntegral (n :: Word32)
    let sha1s = fanout `plusPtr` (256 * 4)
        crcs = sha1s `plusPtr` (n' * 20)
        offsets = crcs `plusPtr` (n' * 4)
        offset64s = offsets `plusPtr` (n' * 4)
    return (IDX2 n' fanout sha1s crcs offsets offset64s)
    
