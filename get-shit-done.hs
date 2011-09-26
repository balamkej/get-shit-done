--This is an implementation of the get-shit-done script in haskell.

import System.Environment 
import System.Directory
import System.Process 
import System.IO 
import Data.List
import Control.Exception 
import Control.Monad

dispatch :: String -> [String] -> IO ()
dispatch "work" = work
dispatch "play" = play
dispatch "view" = view
dispatch "add" = add
dispatch "remove" = remove

main = do
	(command:argList) <- getArgs
	dispatch command argList

hosts = "test_hosts.txt"
blacklist = "blacklist.txt" 
startToken = "## startToken"
endToken = "## endToken"

work :: [String] -> IO ()
work [] = do
	blacklistContents <- readFile blacklist
	hostsContents <- readFile hosts 
	let sites = lines blacklistContents
	    hostsList = lines hostsContents  
	    formattedSites = map (\line -> "127.0.0.1\t" ++ line) sites
	    bookendedSites = [startToken] ++ formattedSites ++ [endToken]
	when (startToken `elem` hostsList) $ error "Work mode already set."
	appendFile hosts (unlines bookendedSites)
	
play :: [String] -> IO ()
play [] = do
	hostsContents <- readFile hosts
	let hostsList = lines hostsContents
	when (not (startToken `elem` hostsList)) $ error "Play mode already set."
	let breakList = break (==startToken) hostsList
	    newHosts = unlines $ (fst breakList)
	(tempName, tempHandle) <- openTempFile "." "temp"
	hPutStr tempHandle newHosts
	hClose tempHandle
	removeFile hosts
	renameFile tempName hosts	 

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
	removeFile blacklist
	renameFile tempName blacklist	
