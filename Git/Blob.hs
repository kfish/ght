module Git.Blob (
    readBlob
  , prettyBlob
  , findBlob
) where

import Codec.Compression.Zlib
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

-- show-prefix, show-root use these
import System.FilePath
import System.Directory
import System.Posix.Files

import Git.Commit
import Git.Path

------------------------------------------------------------

readBlob blob = do
        let (bH,bT) = splitAt 2 blob
        path <- gitPath ("objects" </> bH </> bT)
	b <- L.readFile path
	return (decompress b)

prettyBlob blob bs
	| commitHeader `L.isPrefixOf` bs = C.concat [commitHeader, C.pack (blob ++ "\n"), commitPretty $ commitParse bs]
        | otherwise = chomp bs
        where
                commitHeader = C.pack "commit "
                chomp = C.takeWhile (/= '\n')

------------------------------------------------------------
-- findBlob
--

findBlob [] = findBlob ["HEAD"]

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
firstExist [] = return Nothing
firstExist (f:fs) = do
	p <- gitPath f
	b <- fileExist p
	if b then return (Just f) else firstExist fs

