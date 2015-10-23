import System.IO
import qualified System.Time as Time
import qualified Data.Time.Clock.POSIX as POSIXClock
import System.Environment
import Control.Concurrent
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Network.HTTP.Conduit
import Text.Printf
import Control.Monad
import Data.List
--main = simpleHttp "http://localhost/mirror/C.pdf" >>= (BL.writeFile "test_007.pdf")


-- Returns the current Unix timestamp 
-- Examples:
--
-- > getCurrentTime >>= (\ts -> putStrLn ("Current timestamp is " ++ (show ts)))
getCurrentTime :: IO Integer
getCurrentTime = round `fmap` POSIXClock.getPOSIXTime


data Resource = Resource {
	urlString :: String,
	fromIndex :: Int,
	toIndex	  :: Int,
	complete  :: Bool,
	timeStamp :: Integer
} deriving(Show)


-- Creates a new 'Resource'. Takes a 'urlString' and the byte range as the input
-- Timestamp is added by default as the current UNIX timestamp
--
-- Examples:
-- > mkResource "http://mirror.com/db.pdf" 0 56
mkResource :: String -> Int -> Int -> IO (Resource)
mkResource urlString from to = do
	ts <- getCurrentTime
	return (Resource urlString from to False ts)


-- 'ResourceQueue' represents a 'Queue' of waiting resources to be
-- downloaded from the network.
-- A resource represents an object that can be downloaded from a network.  
-- see 'Resource' data type for more imformation
data ResourceQueue =  EmptyQueue
					| ResourceQueue [Resource] deriving(Show)


-- This newtype represents a state of 'ResourceQueue' at some instance of time
-- 'ResourceQueueState' is basically an 'MVar' with a 'ResourceQueue' inside.
-- 'ResourceQueueState' represents a thread safe 'Queue' data structure that 
-- supports concurrent 'inserts' and 'poll' operations.
-- See 'insertIntoResourceQueue' and 'pollFromResourceQueue' functions for more 
-- details
newtype ResourceQueueState = ResourceQueueState (MVar ResourceQueue)


-- Wraps a list of resources into a 'ResourceQueueState' 
-- Examples:
--
-- First create a new resource
-- > let r = mkResource "http://localhost/mirror/C.pdf" 32 6400
-- Creating a ResourceQueueState
-- > let rqs = (r >>= (\res -> newResourceQueue [res]))
newResourceQueue :: [Resource] -> IO (ResourceQueueState)
newResourceQueue resList = do
	m <- newMVar (ResourceQueue resList)
	return (ResourceQueueState m)


insertIntoResourceQueue :: ResourceQueueState -> Resource -> IO ()
insertIntoResourceQueue (ResourceQueueState m) res = do
	resQueue <- takeMVar m
	case resQueue of 
		(ResourceQueue l) -> do
			putMVar m (ResourceQueue (l ++ [res]))
		EmptyQueue -> do
			putMVar m (ResourceQueue [res])


pollFromResourceQueue :: ResourceQueueState -> IO (Maybe Resource)
pollFromResourceQueue (ResourceQueueState m) = do
	resQueue <- takeMVar m
	case resQueue of 
		(ResourceQueue (r:rs)) -> do
			putMVar m (ResourceQueue rs)
			return (Just r)
		(ResourceQueue []) -> do
			putMVar m (ResourceQueue [])
			return Nothing
		EmptyQueue -> do
			putMVar m (ResourceQueue [])
			return Nothing

--cp :: FilePath -> FilePath -> IO ()
--cp src dest = do
--	contents <- BL.readFile src 
--	BL.writeFile dest contents
handler :: Resource -> IO ()
handler res = do
	putStrLn ("I'm about to handle following resource: " ++ show(res))
	putStrLn "When I wake up after 10 seconds"
	threadDelay(10^6 * 10)
	putStrLn ((urlString res) ++ " handled successfully!")

main = do
	resQueue <- newResourceQueue []
  	forever $ do
    	s <- getLine    
    	res <- mkResource s 0 125       
    	insertIntoResourceQueue resQueue res
    	forkIO $ do 
    		resMonad <- pollFromResourceQueue resQueue
    		case resMonad of 
    			(Just r) -> (handler r)
    			Nothing -> putStrLn "No resource to work on, bye"

setReminder :: String -> IO ()
setReminder s  = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)                   -- 3
  printf "%d seconds is up! BING!\BEL\n" t -- 4