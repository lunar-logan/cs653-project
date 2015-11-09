import System.Environment
import System.IO
import Text.Printf
import Network
import Control.Concurrent
import Control.Monad	
import Data.ByteString
import qualified Data.List as L
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as CB
import Data.Word

sameBytes :: [Word8] -> [Word8] -> Bool
sameBytes [] [] = True
sameBytes _ [] = False
sameBytes [] _ = False
sameBytes (p:ps) (q:qs) 
	| p == q 	= True && (sameBytes ps qs)
	| otherwise = False


readUntil :: [Word8] -> [Word8] -> IO [Word8]
readUntil [] a = return a
readUntil a [] = return []
readUntil bytePattern byteStr = do
	let patSize = L.length bytePattern
	let bytes = L.take patSize byteStr
	--System.IO.putStrLn $ show bytes
	if sameBytes bytePattern bytes then do
		return []
	else do
		r <- readUntil bytePattern $ L.tail byteStr
		return ((L.head byteStr):r)


parseReqLine :: [Char] -> IO ()
parseReqLine reqLine = do
	let version:code:rest = L.words reqLine
	System.IO.putStrLn ("Http Version : " ++ version)
	System.IO.putStrLn ("Response code: " ++ code)


parseHeaders :: [String] -> IO [(String, String)]
parseHeaders [] = return []
parseHeaders (headerLine:hs) = do
	case L.elemIndex ':' headerLine of 
		(Just idx) -> do
			let (k,v') = L.splitAt idx headerLine
			case L.stripPrefix ": " v' of 
				Just v -> do
					rest <- parseHeaders hs
					return ([(k,v)] ++ rest)
				Nothing -> return []
		Nothing -> return []


parseRequest :: [Word8] -> IO ()
parseRequest byteStr = do
	let req' = CB.unpack (B.pack byteStr)
	let req = L.filter (/='\r') req'
	let reqLines = L.lines req
	parseReqLine $ L.head reqLines
	headers <- parseHeaders $ L.tail reqLines
	System.IO.putStrLn $ show headers


processHTTPRequest :: B.ByteString -> IO B.ByteString
processHTTPRequest bs = do
	let asBytes = B.unpack bs
	reqBytes <- readUntil [13, 10, 13, 10] asBytes
	let reqBytesCount = L.length reqBytes
	parseRequest reqBytes -- parsing the http request
	let bodyBytes = L.drop (reqBytesCount+4) asBytes -- represents the body bytes
	return (B.pack bodyBytes)


data HTTPResponse = HTTPResponse {
	statusCode 		:: Int,
	statusPhrase 	:: String,
	contentLength 	:: Int,
	contentType 	:: String

} deriving(Show)


printBytes :: [Word8] -> IO ()
printBytes bytes = System.IO.putStrLn $ show (B.pack bytes)

main :: IO ()
main = do
	bs <- B.readFile "test.txt"
	bb <- processHTTPRequest bs
	System.IO.putStrLn $ show bb
	--reqBytes <- readUntil [13,10,13,10] (B.unpack bs)
	--reqLine <- parseHTTPHeaders $ B.unpack bs'
	--printBytes reqBytes
	--parseRequest reqBytes
	--let (w,rest) = nextToken $ B.unpack bs'
	--System.IO.putStrLn "First word is: "
	--printBytes w
	--let (w',rest') = nextToken rest
	--System.IO.putStrLn "Second word is: "
	--printBytes w'
	--let (w'',rest'') = nextToken rest'
	--System.IO.putStrLn "Third word is: "
	--printBytes w''

	--let (w''',rest''') = nextToken rest''
	--System.IO.putStrLn "Fourth word is: "
	--printBytes w'''


--talk :: Handle -> IO ()
--talk handle = do
--	bs <- B.hGetContents handle 
--	System.IO.putStrLn ("Read a block " ++ (show bs))
--	System.IO.putStrLn ("test: " ++ (show $ parseHTTPHeaders bs))


--main = withSocketsDo $ do
--	sock <- listenOn (PortNumber (fromIntegral port))
--	printf "Listening on port %d\n" port
--	forever $ do
--		(handle,host,port) <- accept sock
--		printf "Accepted connection from %s: %s\n" host (show port)
--		forkFinally (talk handle) (\_ -> hClose handle) 

--port :: Int
--port = 32457

--cp = do
--	content <- B.readFile "p.zip"
--	B.writeFile "q.zip" content



--requestsGET :: String -> IO ByteString
--requestsGET urlString = do
--	rsp <- Network.HTTP.simpleHTTP (getRequest urlString)
--	content <- getResponseBody rsp
--	return content
