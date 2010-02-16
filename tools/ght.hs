module Main where

import Control.Monad (liftM, when)
import Control.Monad.Trans (liftIO)

import Data.Default
import Data.List (intersperse)

import UI.Command

-- show-prefix, show-root use these
import System.FilePath hiding (normalise)
import System.Directory
import System.Posix.Files

-- show
import System.IO (stdout)
import Codec.Compression.Zlib
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (sha1, showDigest)

------------------------------------------------------------
-- show-prefix
--

ghtShowPrefix :: Command ()

ghtShowPrefix = defCmd {
       	        cmdName = "show-prefix",
                cmdHandler = ghtShowPrefixHandler,
                cmdCategory = "Reporting",
                cmdShortDesc = "Show path from top-level directory of repo"
        }

ghtShowPrefixHandler = do
	path <- liftIO findRoot
	cwd <- liftIO $ getCurrentDirectory
	canPath <- liftIO $ canonicalizePath path
	let relPath = makeRelative canPath cwd
	liftIO $ putStrLn (relPath ++ [pathSeparator])

------------------------------------------------------------
-- show-root
--

ghtShowRoot :: Command ()

ghtShowRoot = defCmd {
       	        cmdName = "show-root",
                cmdHandler = ghtShowRootHandler,
                cmdCategory = "Reporting",
                cmdShortDesc = "Show path to top-level directory of repo"
        }

ghtShowRootHandler = do
	path <- liftIO findRoot
	liftIO $ putStrLn path

findRoot :: IO FilePath
findRoot = do
	mp <- liftIO $ findRoot' "."
	case mp of
		Just path -> return (path ++ [pathSeparator])
		Nothing -> error "fatal: Not a git repository (or any of the parent directories)"

findRoot' :: FilePath -> IO (Maybe FilePath)
findRoot' path = do
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
					if (canPath == canNewPath) then
						return Nothing
						else findRoot' newPath
		False -> return Nothing
	
dirIsRoot path = do
	let dotGit = path </> ".git"
	liftIO $ fileExist dotGit
	
------------------------------------------------------------
-- normalise
--

-- NOTE: this is a modified version of normalise from filepath,
-- fixed to handle the case of a trailing dot. I sent a patch
-- with this change to ndm on 20100211; once that is upstream,
-- then this copy can be removed and the System.FilePath version
-- used instead.

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

normalise :: FilePath -> FilePath
normalise path = joinDrive (normaliseDrive drv) (f pth)
              ++ [pathSeparator | not (null pth) && isPathSeparator (last pth)]
    where
        (drv,pth) = splitDrive path

        f = joinPath . dropDots [] . splitDirectories . propSep

        propSep (a:b:xs)
         | isPathSeparator a && isPathSeparator b = propSep (a:xs)
        propSep (a:xs)
         | isPathSeparator a = pathSeparator : propSep xs
        propSep (x:xs) = x : propSep xs
        propSep [] = []

        dropDots acc ["."] = ["."]
	dropDots acc xs = dropDots' acc xs

        dropDots' acc (".":xs) = dropDots' acc xs
        dropDots' acc (x:xs) = dropDots' (x:acc) xs
        dropDots' acc [] = reverse acc

--joinDrive = ++
normaliseDrive = id

------------------------------------------------------------
-- show
--

ghtShow = defCmd {
       	        cmdName = "show",
                cmdHandler = ghtShowHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "show a blob",
                cmdExamples = [("Show contents of blob deadbeef", "deadbeef")]
        }

ghtShowHandler = do
        args <- appArgs
        liftIO $ showBlob args

showBlob [] = do
	putStrLn "Show default revision"
	
showBlob (blob:_) = do
	root <- findRoot
        let (bH,bT) = splitAt 2 blob
        let path = root </> ".git" </> "objects" </> bH </> bT
	b <- L.readFile path
	L.hPut stdout (decompress b)

------------------------------------------------------------
-- hash-object
--

ghtHashObject = defCmd {
       	        cmdName = "hash-object",
                cmdHandler = ghtHashObjectHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Compute object ID from a file",
                cmdExamples = [("Compute the object ID of file.c", "file.c")]
        }

ghtHashObjectHandler = do
        args <- appArgs
	liftIO $ hashFile args

hashFile [] = return ()

hashFile (path:_) = do
        b <- L.readFile path
	status <- getFileStatus path	
	let h = C.pack $ "blob " ++ (show $ fileSize status)
	let t = h `L.append` (L.singleton 0x0) `L.append` b
        putStrLn $ showHash t

showHash = showDigest . sha1

{-
------------------------------------------------------------
-- log
--

ghtLog :: Command ()

ghtLog = defCmd {
       	        cmdName = "log",
                cmdHandler = ghtLogHandler,
                cmdCategory = "Reporting",
                cmdShortDesc = "Show commit logs"
        }

ghtLogHandler = liftIO $ putStrLn "Hello log!"

------------------------------------------------------------
-- formatPatch
--

ghtFormatPatch = defCmd {
       	        cmdName = "format-patch",
                cmdHandler = ghtFormatPatchHandler,
                cmdCategory = "Patch handling",
                cmdShortDesc = "Prepare patches for e-mail submission",
                cmdExamples = [("Create 7 patches", "7"), ("Create 3 patches", "3")]
        }

ghtFormatPatchHandler = do
        args <- appArgs
        when (args == []) $ return ()
        liftIO $ putStrLn $ concat . intersperse " " $ take (read $ head args) (repeat "patch!")
-}

------------------------------------------------------------
-- The Application
--

ght :: Application () ()
ght = def {
	        appName = "ght",
                appVersion = "0.1",
		appAuthors = ["Conrad Parker"],
                appBugEmail = "conrad@metadecks.org",
                appShortDesc = "Trivial git inspection tools",
                appLongDesc = longDesc,
	        appCategories = ["Reporting", "Blob management"],
		appSeeAlso = ["git"],
		appProject = "Ght",
	        appCmds = [ghtShowPrefix, ghtShowRoot, ghtShow, ghtHashObject]
	}

longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain ght
