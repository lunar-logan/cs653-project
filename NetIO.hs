module NetIO 
( nioReadHTTP
, nioWriteHTTP
, mkRangeHeader
, nioGetRequest
, nioReadHTTP'
) where 

import System.IO
import Network.HTTP
import Network.URI
import Network.Stream
import Network.BufferType ( BufferOp(..), BufferType(..) )

{-
	Given the byte range, this function creates the "range" header of the HTTP request
	According to IETF specs range header is of the following format:

	Range: bytes=<fromByteIndex>-[<toByteIndex>]

	Provided the server supports partial content (206) in bytes. Fortuantely, most
	HTTP/1.1 complaint servers do accept ranges in bytes.
-}
mkRangeHeader :: Int -> Int -> Header 
mkRangeHeader fromBytes toBytes =
	if toBytes < fromBytes then 
		error "Invalid byte range"
	else 
		mkHeader HdrRange ("bytes=" ++ (show fromBytes) ++ "-" ++ (show toBytes))

{- 
	Copied from the following URL:
	https://hackage.haskell.org/package/HTTP-4000.2.20/docs/src/Network-HTTP-Base.html#mkRequest
-}
toBufOps :: BufferType a => Request a -> BufferOp a
toBufOps _ = bufferOps

-- Returns a 'Request' object given the url as a 'String' and a list of 
-- HTTP headers 
nioGetRequest :: BufferType ty => String -> [Header] -> Request ty
nioGetRequest urlString headers = 
	case parseURI urlString of
		Nothing -> error ("headRequest: Not a valid URL - " ++ urlString)
		Just url -> req
			where 
				req = 
					Request { rqURI		= url
							, rqBody	= empty
							, rqHeaders	= headers
							, rqMethod	= GET	 	
							}
				empty = buf_empty (toBufOps req)
		

-- Reads bytes from an HTTP server. 
-- Takes url and a list of HTTP headers as input, last param *must* always be 0
nioReadHTTP :: FilePath -> [Header] -> Int -> IO String
nioReadHTTP urlString headers nBytes = do
	rsp <- Network.HTTP.simpleHTTP (nioGetRequest urlString headers)
	content <- getResponseBody rsp
	return content


-- Helper function to read byte ranges from HTTP servers. 
nioReadHTTP' :: String -> Int -> Int -> IO String
nioReadHTTP' urlString fromBytes toBytes = do
	nioReadHTTP urlString headers 0
	where headers = [mkRangeHeader fromBytes toBytes]


nioWriteHTTP :: FilePath -> String -> String
nioWriteHTTP path text = 
	"Function not yet implemented"