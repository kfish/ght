{-# OPTIONS -Wall #-}

module Git.Commit (
	Commit(..),
	commitParse,
	commitPretty
) where

import qualified Data.ByteString.Lazy.Char8 as C

import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

------------------------------------------------------------

data Commit = Commit {
	commitParent :: C.ByteString, --Digest,
	commitAuthor :: C.ByteString, --Person,
	commitAuthorDate :: C.ByteString, --Date,
	commitCommitter :: C.ByteString, --Person,
	commitCommitterDate :: C.ByteString, --Date,
	commitMessage :: C.ByteString
}

------------------------------------------------------------
-- commitParse
--

defCommit :: Commit
defCommit = Commit e e e e e e
    where
        e = C.empty

commitParse :: C.ByteString -> Commit
commitParse bs = commitParseLines defCommit (C.lines bs)

commitParseLines :: Commit -> [C.ByteString] -> Commit
commitParseLines c [] = c
commitParseLines c (l:ls)
	| C.null l = c{commitMessage = C.unlines ls}
	| otherwise = commitParseLines (commitModLine c l) ls

commitModLine :: Commit -> C.ByteString -> Commit
commitModLine c l = commitMod c (C.unpack hd) bdy
	where (hd:bdy) = C.words l

commitMod :: Commit -> [Char] -> [C.ByteString] -> Commit
commitMod c hd bdy
	| hd == "commit" = c
	| hd == "parent" = c{commitParent = head bdy}
	| hd == "author" = c{commitAuthor = pName, commitAuthorDate = t}
	| hd == "committer" = c{commitCommitter = pName, commitCommitterDate = t}
	| otherwise = c
	where
		(pTZ:pTime:pAs) = reverse bdy
		pName = C.unwords $ reverse pAs
		t = C.unwords [pTime, pTZ]

------------------------------------------------------------
-- commitPretty
--

commitPretty :: Commit -> C.ByteString
commitPretty (Commit _p a ad _c _cd m) =
	C.unlines [
		C.concat [(C.pack "Author: "), a],
		C.concat [(C.pack "Date:   "), f],
		C.empty,
		m
	]
	where
		[ps, tz] = map C.unpack $ C.words ad
		s = readTime defaultTimeLocale "%s" ps
		z = utcToZonedTime (read tz) s
		f = C.pack $ formatTime defaultTimeLocale "%c" z
