{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Git.PackIndex (
        dumpRawPackIndex,
        findInPackIdxs,

        -- * Paths
        idxPath
) where

import Control.Applicative ((<$>))
import Control.Monad (msum)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Foreign.Ptr
import Foreign.Storable
import Data.Storable.Endian
import System.Directory
import System.FilePath
import System.IO.MMap
import System.Posix.Types
import Text.Printf

import Git.SHA
import Git.Pack
import Git.Path

------------------------------------------------------------

data IDX = IDX1 {
      idx1Pack       :: FilePath
    , idx1Size       :: Int
    , idx1Fanout     :: Ptr (BigEndian Word32)
    , idx1Offsets    :: Ptr (BigEndian Word32)
    } | IDX2 {
      idx2Pack       :: FilePath
    , idx2Size       :: Int
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

-- | Corresponding packfile path
idxPack :: IDX -> FilePath
idxPack IDX1{..} = idx1Pack
idxPack IDX2{..} = idx2Pack

-- | Number of objects in the corresponding .pack file
idxSize :: IDX -> Int
idxSize IDX1{..} = idx1Size
idxSize IDX2{..} = idx2Size

-- | Nth SHA1
idxSha1 :: IDX -> Int -> IO BS.ByteString
idxSha1 idx@IDX1{..} n
    | n > idx1Size = outOfRange idx n "(v1) SHA1"
    | otherwise     = do
        let cs = idx1Offsets `plusPtr` (4 + (n * 24))
        BS.packCStringLen (cs, 20)
idxSha1 idx@IDX2{..} n
    | n > idx2Size = outOfRange idx n "SHA1"
    | otherwise     = do
        let cs = idx2SHA1s `plusPtr` (n * 20)
        BS.packCStringLen (cs, 20)

-- | Nth CRC
idxCRC :: IDX -> Int -> IO (Maybe Word32)
idxCRC idx@IDX1{..} n
    | n > idx1Size = outOfRange idx n "(v1) CRC"
    | otherwise     = return Nothing
idxCRC idx@IDX2{..} n
    | n > idx2Size = outOfRange idx n "CRC"
    | otherwise     = do
        BE crc <- peekElemOff idx2CRCs n
        return (Just crc)

-- | Nth offset
idxOffset :: IDX -> Int -> IO FileOffset
idxOffset idx@IDX1{..} n
    | n > idx1Size = outOfRange idx n "(v1) Offset"
    | otherwise     = do
        BE off <- peekByteOff idx1Offsets (n * 24)
        return . fromIntegral $ (off :: Word32)
idxOffset idx@IDX2{..} n
    | n > idx2Size = outOfRange idx n "Offset"
    | otherwise     = do
        BE off <- peekElemOff idx2Offsets n
        return . fromIntegral $ off

outOfRange :: IDX -> Int -> String -> IO a
outOfRange idx n s = error $ printf "%s: %s index %d out of range (size %d)"
                                 (idxPack idx) s n (idxSize idx)

------------------------------------------------------------

idxFiles :: IO [FilePath]
idxFiles = do
    packDir <- gitPath ("objects" </> "pack")
    map (packDir </>) . filter isIdx <$> getDirectoryContents packDir
    where
        isIdx = (== ".idx") . takeExtension

------------------------------------------------------------

idxFind :: IDX -> BS.ByteString -> IO (Maybe (IDX, Int))
idxFind idx sha = idxFind' 0 (idxSize idx)
    where
        idxFind' lo hi
            | lo >= hi = do
                iSha <- idxSha1 idx lo
                case (sha `compare` iSha) of
                    EQ -> return (Just (idx, lo))
                    _  -> return Nothing
            | otherwise = do
                iSha <- idxSha1 idx i
                case (sha `compare` iSha) of
                    EQ -> return (Just (idx, i))
                    LT -> idxFind' lo i
                    GT -> idxFind' (i+1) hi
            where
                i = floor ((fromIntegral (lo + hi)) / 2.0 :: Double)

findInPackIdxs :: BS.ByteString -> IO (Maybe PackObject)
findInPackIdxs sha = do
    idxs <- idxFiles
    msum <$> mapM (findInPackIndex' sha) idxs

findInPackIndex' :: BS.ByteString -> FilePath -> IO (Maybe PackObject)
findInPackIndex' sha fp = do
    idx <- readIdx fp
    m'i <- idxFind idx sha
    case m'i of
        Just (_, i) -> do
            off <- idxOffset idx i
            packReadObject (idxPack idx) off
        Nothing     -> return Nothing

------------------------------------------------------------
-- Debugging

dumpIdx :: IDX -> IO ()
dumpIdx idx@IDX1{..} = do
    putStrLn $ idx1Pack ++ ": IDX Version 1"
    dumpIdx' idx
dumpIdx idx@IDX2{..} = do
    putStrLn $ idx2Pack ++ ": IDX Version 2"
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

readIdx :: FilePath -> IO IDX
readIdx fp = do
    (ptr, _rawsize, offset, size) <- mmapFilePtr fp ReadOnly Nothing
    let start :: Ptr (BigEndian Word32)
        start = ptr `plusPtr` offset
    BE hdr <- peek start
    if (hdr == idxHeader)
        then do
            BE ver <- peekElemOff start 1
            case ver of
                2 -> mkIDX2 fp start size
                _ -> error "Unknown version"
        else mkIDX1 fp start size

dumpRawPackIndex :: FilePath -> IO String
dumpRawPackIndex fp = do
    idx <- readIdx fp
    dumpIdx idx
    return "Woot"

mkIDX1 :: FilePath -> Ptr (BigEndian Word32) -> Int -> IO IDX
mkIDX1 fp start _size = do
    let pack = replaceExtension fp ".pack"
        fanout = start
    BE n <- peekElemOff fanout 255
    let n' = fromIntegral (n :: Word32)
    let offsets = fanout `plusPtr` (256 * 4)
    return (IDX1 pack n' fanout offsets)

mkIDX2 :: FilePath -> Ptr (BigEndian Word32) -> Int -> IO IDX
mkIDX2 fp start _size = do
    let pack = replaceExtension fp ".pack"
        fanout = start `plusPtr` (2 * 4)
    BE n <- peekElemOff fanout 255
    let n' = fromIntegral (n :: Word32)
    let sha1s = fanout `plusPtr` (256 * 4)
        crcs = sha1s `plusPtr` (n' * 20)
        offsets = crcs `plusPtr` (n' * 4)
        offset64s = offsets `plusPtr` (n' * 4)
    return (IDX2 pack n' fanout sha1s crcs offsets offset64s)
    
