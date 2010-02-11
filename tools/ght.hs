module Main where

import Control.Monad (liftM, when)
import Control.Monad.Trans (liftIO)

import Data.Default
import Data.List (intersperse)

import UI.Command

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

------------------------------------------------------------
-- The Application
--

ght :: Application () ()
ght = def {
	        appName = "ght",
                appVersion = "0.1",
		appAuthors = ["Joe R. Hacker"],
                appBugEmail = "bugs@example.com",
                appShortDesc = "Ght revision control system",
                appLongDesc = longDesc,
	        appCategories = ["Reporting", "Patch handling"],
		appSeeAlso = ["git"],
		appProject = "Ght",
	        appCmds = [ghtLog, ghtFormatPatch]
	}

longDesc = "A clone of the git revision control system."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain ght
