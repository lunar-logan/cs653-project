import Network.Socket
import Control.Concurrent
import System.Directory
import System.IO
import Data.Hashable 
import Numeric
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString as B
import NetIO
--import Network.HTTP.Conduit
--import qualified Data.ByteString.Lazy as L

--readFileContent :: FilePath -> IO String
--readFileContent filePath = do
--	handle <- openFile filePath ReadMode
--	content <- hGetContents handle
--	return content
	--hClose handle

mkFileName :: String -> Int -> Int -> String
mkFileName urlString fromIndex toIndex = 
	(show (hash urlString)) ++ "_" ++ (show fromIndex) ++ "_" ++ (show toIndex)

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (sock, addr) = do
	handle <- socketToHandle sock ReadMode
	hSetBuffering handle LineBuffering
	readInfo <- hGetContents handle
	putStrLn readInfo
	let [urlString, fromIndex', toIndex'] = words readInfo
	let fromIndex = read fromIndex' :: Int
	let toIndex = read toIndex' :: Int
	firstFewbytes <- NetIO.nioReadHTTP' urlString fromIndex (-1)
	--putStrLn firstFewbytes
	let fileName = (mkFileName urlString fromIndex toIndex)
	B.writeFile fileName firstFewbytes
	hClose handle
--main :: IO ()
--main = do
--	text <- readFileContent "../scrap.hs"
--	putStrLn text
--	return ()

--t = forkIO (writeFile "xyzzzy" "seo craic nua!") >> doesFileExist "xyzzzy"
main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 32457 iNADDR_ANY)
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    -- accept one connection and handle it
    conn <- accept sock
    --runConn conn
    forkIO (handleConnection conn)
    mainLoop sock
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hi!\n"
    sClose sock