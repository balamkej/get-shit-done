--This is an implementation of the get-shit-done (github.com/leftnode/get-shit-done) script in haskell. It particular, it is set up to play well with dnsmasq in mac os x lion.

import System.Environment 
import System.Directory
import System.Cmd 
import System.Exit
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
dispatch "clear" = clear
dispatch "help" = help

main = do
	(command:argList) <- getArgs
	dispatch command argList

hosts = "/usr/local/etc/dnsmasq.conf"
blacklist = "/Users/roberthenderson/.blacklist.conf" 
startToken = "## startToken"
endToken = "## endToken"

help :: [String] -> IO ()
help [] = do
	let helpstr = "work:\t" ++ "enter work mode\n" ++ "play:\t" ++ "enter play mode\n" ++ "view:\t" ++ "view blocked sites\n" ++ "add:\t" ++ "add site to blocked sites\n" ++ "remove:\t" ++ "remove site N from blocked sites\n" ++ "clear:\t" ++ "reset dnsmasq\n" ++ "help:\t" ++ "shows options"
	putStrLn helpstr
	
work :: [String] -> IO ()
work [] = do
	blacklistContents <- readFile blacklist
	hostsContents <- readFile hosts 
	let sites = lines blacklistContents
	    hostsList = lines hostsContents  
	    formattedSites = map (\line -> "address=/" ++ line ++ "/127.0.0.1") sites
	    bookendedSites = [startToken] ++ formattedSites ++ [endToken]
	when (startToken `elem` hostsList) $ error "Work mode already set."
	appendFile hosts (unlines bookendedSites)

clear :: [String] -> IO ()	
clear [] = do 
	ecode <- rawSystem "launchctl" ["remove", "uk.org.thekelleys.dnsmasq"]
	if ecode == ExitSuccess
		then cache
		else putStrLn "There was a problem stopping the DNS server."

cache = do 
	ecode <- rawSystem "dscacheutil" ["-flushcache"]
	if ecode == ExitSuccess
		then dns
		else putStrLn "There was a problem clearing the DNS cache."

dns = do 
	ecode <- rawSystem "launchctl" ["load", "-w", "/System/Library/LaunchDaemons/uk.org.thekelleys.dnsmasq.plist"]
	if ecode == ExitSuccess
		then putStrLn "DNS cache cleared and server reset. Don't forget to restart your browser."
		else putStrLn "There was a problem restarting the DNS server."

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
