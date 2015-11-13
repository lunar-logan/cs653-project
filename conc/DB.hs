{-# LANGUAGE OverloadedStrings #-}
module DB () where



import           Control.Applicative
import Control.Monad
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

--data TestField = TestField Int T.Text deriving (Show)

--instance FromRow TestField where
--  fromRow = TestField <$> field <*> field

--instance ToRow TestField where
--  toRow (TestField id_ str) = toRow (id_, str)
data TestField = TestField Int T.Text T.Text Int deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field <*> field

instance ToRow TestField where
  toRow (TestField id_ url file_name length) = toRow (id_, url, file_name, length)

dbName :: String
dbName = "download.db"

storeRecord :: String -> String -> Int -> IO ()
storeRecord urlString fileName length = do
  conn <- open dbName
  execute conn "INSERT INTO files (url, file_name, length) VALUES (?,?,?)" (urlString :: String, fileName :: String, length :: Int)
  close conn


deleteRecord :: Int -> IO ()
deleteRecord id = do
  conn <- open dbName
  execute conn "DELETE FROM files WHERE id=?" (Only id)
  close conn


dumpRecord :: TestField -> IO ()
dumpRecord fields = do
  let (TestField id url name len) = fields
  putStrLn $ show id


--fetchRecord :: IO ()
fetchRecord callback = do
  conn <- open dbName 
  xs <- query_ conn "select * from files" :: IO [TestField]
  mapM_ callback xs
  close conn




main :: IO ()
main = do
  conn <- open dbName
  execute_ conn "CREATE TABLE IF NOT EXISTS files (id INTEGER PRIMARY KEY, url TEXT, file_name TEXT, length INTEGER)"
  close conn