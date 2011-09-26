--This is an implementation of the get-shit-done script in haskell.

import System.Environment 
import System.Directory 
import System.IO 
import Data.List
import Control.Exception 

dispatch :: String -> [String] -> IO ()
--dispatch "work" = work
--dispatch "play" = play
dispatch "view" = view
dispatch "add" = add
dispatch "remove" = remove

main = do
	(command:argList) <- getArgs
	dispatch command argList

hosts = "test_hosts.txt"
blacklist = "blacklist.txt" 

view :: [String] -> IO ()
view [] = do
	contents <- readFile blacklist
	let sites = lines contents
	    numberedSites = zipWith (\n line -> show n ++ " - " ++ line) [0..] sites
	putStr $ unlines numberedSites 

add :: [String] -> IO ()
add [site] = appendFile blacklist (site ++ "\n")

remove :: [String] -> IO ()
remove [numberString] = do
	contents <- readFile blacklist
	let sites = lines contents
	    number = read numberString
	    newBannedSites = unlines $ delete (sites !! number) sites
	(tempName, tempHandle) <- openTempFile "." "temp"
	hPutStr tempHandle newBannedSites
	hClose tempHandle
	removeFile "blacklist.txt"
	renameFile tempName "blacklist.txt"	
