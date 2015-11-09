import System.IO
import System.Environment
import Text.Printf
import Network
--import Network.Socket
import Control.Concurrent
import Control.Monad	

talk :: Handle -> IO ()
talk h = do
	hSetBuffering h LineBuffering
	loop
	where loop = do
		line <- hGetLine h
		if line == "end"
			then hPutStrLn h "Thanks for using the service"
		else do
			hPutStrLn h (show (2 * (read line :: Integer)))
			loop


main = withSocketsDo $ do
	sock <- listenOn (PortNumber (fromIntegral port))
	printf "Listening on port %d\n" port
	forever $ do
		(handle, host, port) <- accept sock
		printf "Accepted connection from %s: %s\n" host (show port)
		forkFinally (talk handle) (\_ -> hClose handle)    

port :: Int
port = 44444         