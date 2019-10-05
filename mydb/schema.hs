module Schema (
    DataType (..), ColumnSchema (..), TableSchema (..),
    parseTableSchema, printTableSchema
) where

import Data.List.Split (splitOn)
import qualified Data.List as List

data DataType = String | Int | Float
    deriving (Show, Read)

data ColumnSchema = ColumnSchema { 
    columnName :: String, 
    columnDataType :: DataType
} deriving (Show)

data TableSchema = TableSchema {
    tableName :: String,
    tableColumns :: [ColumnSchema],
    primaryKey :: String
} deriving (Show)

parseColumnSchema :: String -> ColumnSchema
parseColumnSchema str = ColumnSchema columnName (read columnDataType)
    where [columnName, columnDataType] = words str

parseTableSchema :: String -> TableSchema
parseTableSchema str = TableSchema tableName (map parseColumnSchema columns) primaryKey
    where (tableName:primaryKey:columns) = splitOn "|" str

printColSchema :: ColumnSchema -> String
printColSchema (ColumnSchema name dataType) = unwords [name, show dataType]

printTableSchema :: TableSchema -> String
printTableSchema (TableSchema name cols key) = List.intercalate "|" (name:key:map printColSchema cols)