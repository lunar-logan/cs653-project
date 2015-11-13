import qualified Data.ByteString as B
import Network.HTTP
import Network.URI (parseURI)

main = do
    jpg <- get "http://www.python.org/ftp/python/3.5.0/python-3.5.0.exe"
    B.writeFile "python3.exe" jpg
  where
    get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u -> u in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody