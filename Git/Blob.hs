module Git.Blob (
    readBlob
  , prettyBlob
  , findBlob
) where

import Codec.Compression.Zlib
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (listToMaybe)

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
        decompress <$> L.readFile path

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
firstExist fs = listToMaybe <$> (filterM fileExist <=< mapM gitPath) fs
