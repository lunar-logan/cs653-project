{- Shri Ganeshaaye Namah: -}
import Network.HTTP 
       -- fetch document and return it (as a 'String'.)

main = do
	rsp <- Network.HTTP.simpleHTTP (getRequest "http://localhost/mirror/p1.txt")
	        -- fetch document and return it (as a 'String'.)
	res <- fmap (take 10000) (getResponseBody rsp)
	putStrLn $ show res
