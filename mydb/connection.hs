{-# LANGUAGE BangPatterns #-}
module Connection (
    Connection (..), connectToDb, query, upsert
) where

import DB (Table, Row)
import qualified DB as DB
import Schema (TableSchema)
import qualified Schema as Schema
import System.IO (Handle)
import qualified System.IO as IO
import qualified Data.Maybe as Maybe
import Control.DeepSeq

data Connection = Connection {
    connFilePath :: String
} deriving (Show)

connectToDb :: String -> IO Connection
connectToDb filePath = return $ Connection filePath

readFileContent :: String -> IO String
readFileContent filePath =
    do
        hdl <- IO.openFile filePath IO.ReadMode
        content <- IO.hGetContents hdl
        content `deepseq` IO.hClose hdl
        return content

readTable :: Connection -> IO (Table, TableSchema)
readTable (Connection filePath) =
    do
        content <- readFileContent filePath
        let (schemaStr:rows) = lines content
        let schema = Schema.parseTableSchema schemaStr
        return $ (DB.parseTable schema rows, schema)

query :: Connection -> String -> IO [Row]
query conn userIdStr = 
    do
        (table, _) <- readTable conn
        return $ Maybe.maybeToList $ DB.select table (DB.IntCell userId)
    where userId = read userIdStr :: Int

upsert :: Connection -> String -> IO Bool
upsert conn rowStr =
    do
        (table, schema) <- readTable conn
        let (newTable, ok) = DB.upsert schema table rowStr
        putStrLn (show ok)
        case ok of
            True -> 
                do
                    --putStrLn (show newTable)
                    let newTableStr = (unlines [Schema.printTableSchema schema, DB.printTable schema newTable])
                    --putStrLn newTableStr
                    IO.withFile (connFilePath conn) IO.WriteMode (`IO.hPutStr` newTableStr)
                    return True
            False -> return False