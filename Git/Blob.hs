{-# OPTIONS -Wall #-}

module Git.Blob (
    readBlob
  , prettyBlob
  , findBlob
) where

import Codec.Compression.Zlib
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (listToMaybe)

-- show-prefix, show-root use these
import System.FilePath
import System.Posix.Files
import System.IO

import Git.Commit
import Git.Pack
import Git.PackIndex
import Git.Path
import Git.SHA

------------------------------------------------------------

readBlob :: String -> IO (Maybe L.ByteString)
readBlob blob = do
        let (bH,bT) = splitAt 2 blob
        path <- gitPath ("objects" </> bH </> bT)
        exists <- fileExist path
        if exists
            then do
                Just . decompress <$> C.readFile path
            else do
                let sha = readDigestBS blob
                m'po <- findInPackIdxs sha
                return $ fmap (packObjectPretty sha) m'po

prettyBlob :: String -> C.ByteString -> C.ByteString
prettyBlob blob bs
	| commitHeader `L.isPrefixOf` bs = C.concat [commitHeader, C.pack (blob ++ "\n"), commitPretty $ commitParse bs]
        | otherwise = chomp bs
        where
                commitHeader = C.pack "commit "
                chomp = C.takeWhile (/= '\n')

------------------------------------------------------------
-- findBlob
--

findBlob :: [String] -> IO [String]
findBlob []       = findBlob ["HEAD"]
findBlob (name:_) = do
	mPath <- firstExist [name,
                            ("refs" </> name),
                            ("refs" </> "tags" </> name),
                            ("refs" </> "heads" </> name),
                            ("refs" </> "remotes" </> name),
                            ("refs" </> "remotes" </> name </> "HEAD")]
	case mPath of
		Just path -> do
			bs <- gitDeref path
			return [C.unpack bs]
		Nothing -> return [name]

firstExist :: [FilePath] -> IO (Maybe FilePath)
firstExist fs = listToMaybe <$> (filterM (fileExist <=< gitPath)) fs
