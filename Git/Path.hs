{-# OPTIONS -Wall #-}

module Git.Path (
  -- * Generate paths in git dir
    gitPath
  , gitRoot
  , gitDeref

  -- * General path handling
  , pathExistOr
) where

import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

-- show-prefix, show-root use these
import System.FilePath hiding (normalise)
import System.Directory
import System.Posix.Files

------------------------------------------------------------
-- gitRoot
--

gitPath :: FilePath -> IO FilePath
gitPath f = do
	root <- gitRoot
	return $ root </> ".git" </> f

gitRoot :: IO FilePath
gitRoot = do
	mp <- liftIO $ gitRoot' "."
	case mp of
		Just path -> return (path ++ [pathSeparator])
		Nothing -> error "fatal: Not a git repository (or any of the parent directories)"

gitRoot' :: FilePath -> IO (Maybe FilePath)
gitRoot' path = do
	b <- fileExist path
	case b of
		True -> do
			d <- dirIsRoot path
			case d of
				True -> return (Just (normalise path))
				False -> do
					let newPath = ".." </> path
					canPath <- canonicalizePath path
					canNewPath <- canonicalizePath newPath
					if (canPath == canNewPath)
						then return Nothing
						else gitRoot' newPath
		False -> return Nothing
    where
        dirIsRoot p = liftIO $ fileExist (p </> ".git")
	
------------------------------------------------------------
-- deref
--

gitDeref :: String ->  IO C.ByteString
gitDeref = deref <=< L.readFile <=< gitPath
    where
        deref bs
	    | refHeader `L.isPrefixOf` bs = gitDeref refPath
            | otherwise = return (chomp bs)
            where
		refHeader = C.pack "ref: "
		refPath = C.unpack (chomp $ L.drop 5 bs)
                chomp = C.takeWhile (/= '\n')

------------------------------------------------------------
-- pathExistOr

-- | Return the given path if it exists, else the result of applying the
-- modifier function
pathExistOr :: (FilePath -> IO FilePath) -> FilePath -> IO FilePath
pathExistOr f path = do
    exists <- doesFileExist path
    if exists
        then return path
        else f path

------------------------------------------------------------
-- normalise
--

-- NOTE: this is a modified version of normalise from filepath,
-- fixed to handle the case of a trailing dot. This version was
-- submitted via the libraries process as ticket #3975:
-- http://hackage.haskell.org/trac/ghc/ticket/3975
-- which was applied on 08 Jan 2011.

-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- > Posix:   normalise "/file/\\test////" == "/file/\\test/"
-- > Posix:   normalise "/file/./test" == "/file/test"
-- > Posix:   normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > Posix:   normalise "../bob/fred/" == "../bob/fred/"
-- > Posix:   normalise "./bob/fred/" == "bob/fred/"
-- > Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > Windows: normalise "c:\\" == "C:\\"
-- > Windows: normalise "\\\\server\\test" == "\\\\server\\test"
-- > Windows: normalise "c:/file" == "C:\\file"
-- >          normalise "." == "."
-- > Posix:   normalise "./" == "./"
-- > Posix:   normalise "./." == "./"
-- > Posix:   normalise "bob/fred/." == "bob/fred/"
normalise :: FilePath -> FilePath
normalise path = joinDrive (normaliseDrive drv) (f pth)
              ++ [pathSeparator | isDirPath pth]
    where
        (drv,pth) = splitDrive path

        isDirPath xs = lastSep xs
            || not (null xs) && last xs == '.' && lastSep (init xs)
        lastSep xs = not (null xs) && isPathSeparator (last xs)

        f = joinPath . dropDots [] . splitDirectories . propSep

        propSep (a:b:xs)
         | isPathSeparator a && isPathSeparator b = propSep (a:xs)
        propSep (a:xs)
         | isPathSeparator a = pathSeparator : propSep xs
        propSep (x:xs) = x : propSep xs
        propSep [] = []

        dropDots _   xs | all (==".") xs = ["."]
        dropDots acc xs = dropDots' acc xs

        dropDots' acc (".":xs) = dropDots' acc xs
        dropDots' acc (x:xs) = dropDots' (x:acc) xs
        dropDots' acc [] = reverse acc

--joinDrive = ++
normaliseDrive :: FilePath -> FilePath
normaliseDrive = id

