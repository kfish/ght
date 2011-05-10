module Main where

import Control.Applicative ((<$>))
import Control.Monad (liftM, when)
import Control.Monad.Trans (liftIO)

import Data.Default
import Data.List (intersperse, sort)

import UI.Command

import Git.Blob
import Git.Commit
import Git.Pack
import Git.PackIndex
import Git.Path

-- show-prefix, show-root use these
import System.FilePath
import System.Directory
import System.Posix.Files

-- show
import System.IO (stdout)
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

ghtShowPrefixHandler = liftIO $ do
	canPath <- canonicalizePath =<< gitRoot
	cwd <- getCurrentDirectory
	let relPath = makeRelative canPath cwd
	putStrLn (relPath ++ [pathSeparator])

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

ghtShowRootHandler = liftIO $ putStrLn =<< gitRoot

------------------------------------------------------------
-- branch
--

ghtBranch = defCmd {
       	        cmdName = "branch",
                cmdHandler = ghtBranchHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "show branches",
                cmdExamples = [("Show branches available", "")]
        }

ghtBranchHandler = liftIO . showBranches =<< appArgs

showBranches _ = do
	path <- gitPath $ "refs" </> "heads"
	branches <- getDirectoryContents path
	let branches' = filter (/= ".") branches
	let branches'' = filter (/= "..") branches'
	hd <- gitDeref "HEAD"
	mapM_ (showBranch hd) (sort branches'')

showBranch hd b = do
	ref <- gitDeref $ "refs" </> "heads" </> b
	if (ref == hd)
		then putStr "* "
		else putStr "  "
	putStrLn b

------------------------------------------------------------
-- log
--

ghtLog = defCmd {
       	        cmdName = "log",
                cmdHandler = ghtLogHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show commit logs",
                cmdExamples = [("Show log of current branch", ""), ("Show log of branch feature1", "feature1")]
        }

ghtLogHandler = liftIO . showLog =<< liftIO . findBlob =<< appArgs

showLog (blob:_)
	| blob == "" = return ()
	| otherwise = do
		d <- readBlob blob
		let m'pb = prettyLog blob d
		case m'pb of
			Just c -> do
				let p = C.concat [commitHeader, C.pack (blob ++ "\n"), commitPretty c]
				L.hPut stdout p
				showLog [C.unpack $ commitParent c]
			Nothing -> return ()
	where
		commitHeader = C.pack "commit "

prettyLog blob bs
	| commitHeader `L.isPrefixOf` bs = Just c
        | otherwise = Nothing
	where
		commitHeader = C.pack "commit "
		c = commitParse bs

------------------------------------------------------------
-- show-pack
--

ghtShowPack = defCmd {
       	        cmdName = "show-pack",
                cmdHandler = ghtShowPackHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show the raw dump of a pack",
                cmdExamples = [("Show raw contents of pack pack-abcd.pack", "abcd")]
        }

ghtShowPackHandler = do
        pack <- (liftIO . fPack =<< appArgs)
        x <- liftIO $ packRead pack
	liftIO $ putStrLn (show x)

fPack (pack:_) = do
    exists <- doesFileExist pack
    if exists
        then return pack
        else packPath pack

------------------------------------------------------------
-- show-idx
--

ghtShowIdx = defCmd {
       	        cmdName = "show-idx",
                cmdHandler = ghtShowIdxHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show the raw dump of a pack index",
                cmdExamples = [("Show raw contents of pack pack-abcd.idx", "abcd")]
        }

ghtShowIdxHandler = do
        idx <- (liftIO . fIdx =<< appArgs)
        x <- liftIO $ dumpRawPackIndex idx
	liftIO $ putStrLn x

fIdx (idx:_) = do
    exists <- doesFileExist idx
    if exists
        then return idx
        else idxPath idx

------------------------------------------------------------
-- show-raw
--

ghtShowRaw = defCmd {
       	        cmdName = "show-raw",
                cmdHandler = ghtShowRawHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show the raw dump of an object",
                cmdExamples = [("Show raw contents of blob deadbeef", "deadbeef"), ("Show raw contents of branch feature1", "feature1")]
        }

ghtShowRawHandler = liftIO . showRawBlob =<< liftIO . findBlob =<< appArgs

showRawBlob (blob:_) = L.hPut stdout =<< readBlob blob

------------------------------------------------------------
-- show
--

ghtShow = defCmd {
       	        cmdName = "show",
                cmdHandler = ghtShowHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show an object",
                cmdExamples = [("Show contents of blob deadbeef", "deadbeef"), ("Show contents of branch feature1", "feature1")]
        }

ghtShowHandler = liftIO . showBlob =<< liftIO . findBlob =<< appArgs

showBlob (blob:_) = L.hPut stdout =<< prettyBlob blob <$> readBlob blob

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

ghtHashObjectHandler = liftIO . hashFile =<< appArgs

hashFile [] = return ()

hashFile (path:_) = do
        b <- L.readFile path
	status <- getFileStatus path	
	let h = C.pack $ "blob " ++ (show $ fileSize status)
	let t = h `L.append` (L.singleton 0x0) `L.append` b
        putStrLn $ showHash t

showHash = showDigest . sha1

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
	        appCmds = [ghtShowPrefix, ghtShowRoot, ghtShow, ghtLog, ghtShowRaw, ghtShowPack, ghtShowIdx, ghtHashObject, ghtBranch]
	}

longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain ght
