import Network.Socket
import Control.Concurrent
import System.Directory
import Control.Monad
import Data.Hashable 
import Numeric
import System.IO 
import qualified Data.ByteString.Lazy as B

import NetIO

-- 'mkFileName' generates a file name based on the url and the byte range
-- Name so generated is of the following format:
--
-- <hash(url)>_<fromIndex>_<toIndex>
-- 
-- Name has no extension. 
-- Examples:
--
-- > (mkFileName "http://dl.binary.in/fzd8" 56 3600) >>= (\n -> putStrLn n)
mkFileName :: String -> Int -> Int -> IO String
mkFileName urlString fromIndex toIndex = do
	return ((show (hash urlString)) ++ "_" ++ (show fromIndex) ++ "_" ++ (show toIndex))


-- Inner function is used within handler function
parsePayloadInfo :: [Char] -> IO (String, Int, Int)
parsePayloadInfo info = do
	let [urlString, fromIndex', toIndex'] = words info
	let fromIndex = read fromIndex' :: Int
	let toIndex = read toIndex' :: Int
	return (urlString, fromIndex, toIndex)


-- Handles a new connection. Process the request to download content 
-- from the web and stores it in the current working directory
-- Download request is of the following format:
-- urlString fromIndex toIndex (all indicies are in bytes)
-- For example `http://dl.binary.in/fzd8 56 3600`. If the 'toIndex' is set to '-1' then
-- all the remaining bytes after 'fromIndex' are downloaded as the 
-- range header is set to: `Range: bytes=5-`
handler :: (Socket, SockAddr) -> IO ()
handler (sock, addr) = do
	handle <- socketToHandle sock ReadMode
	hSetBuffering handle LineBuffering
	payloadInfo <- hGetContents handle
	(urlString, fromIndex, toIndex) <- parsePayloadInfo payloadInfo
	bytes <- NetIO.nioReadHTTP' urlString fromIndex toIndex
	fileName <- mkFileName urlString fromIndex toIndex
	B.writeFile fileName bytes


-- Port at which GetServers listen's for incoming connections
port :: Int 
port = 32457

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0 				-- create socket
    setSocketOption sock ReuseAddr 1				-- set the options
    bindSocket sock (SockAddrInet 32457 iNADDR_ANY)	-- bind to an address
    listen sock 2									-- start listening
    forever $ do
    	newConn <- accept sock -- accept one connection and handle it
    	putStrLn ("Accepted connection at " ++ (show newConn))
    	forkIO (handler newConn) 
 
