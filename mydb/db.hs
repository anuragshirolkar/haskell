module DB (
    Cell (..), Row (..), Table (..),
    upsert, parseTable, select, printTable
) where

import Schema
import qualified Schema
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import qualified Data.List as List

data Cell = 
    StringCell String |
    IntCell Int |
    FloatCell Float
    deriving (Show, Ord, Eq)

data Row = Row {
    rowCells :: Map String Cell
} deriving (Show)

data Table = Table {
    tableRows :: Map Cell Row
} deriving (Show)

upsert :: TableSchema -> Table -> String -> (Table, Bool)
upsert schema table rowStr
    | Maybe.isNothing primaryKeyCell    = (table, False)
    | otherwise                         = (upsert' (Maybe.fromJust primaryKeyCell) row table, True)
    where 
        row = snd $ parseRow schema rowStr
        primaryKeyCell = Map.lookup (Schema.primaryKey schema) (rowCells row)
        upsert' cell row (Table rows) = Table (Map.insert cell row rows)


select :: Table -> Cell -> Maybe Row
select (DB.Table rows) cell = Map.lookup cell rows

parseCell :: DataType -> String -> Cell
parseCell String val = StringCell val
parseCell Int val = IntCell (read val)
parseCell Float val = FloatCell (read val)

parseRow :: TableSchema -> String -> (Cell, Row)
parseRow (TableSchema tableName columns key) rowStr = (keyCell, Row rowMap)
    where
        cellVals = splitOn "|" rowStr
        zipperFn (ColumnSchema name colType) colStr = (name, (parseCell colType colStr))
        rowMap = Map.fromList (zipWith zipperFn columns cellVals)
        keyCell = rowMap Map.! key
        

parseTable :: TableSchema -> [String] -> Table
parseTable schema rows = Table $ Map.fromList (map rowParser rows)
    where 
        rowParser = parseRow schema

printCell :: Cell -> String
printCell (IntCell n) = show n
printCell (StringCell s) = s
printCell (FloatCell f) = show f


printRow :: TableSchema -> Row -> String
printRow (Schema.TableSchema _ cols _) (Row cells) = 
    List.intercalate "|" $ map (printCell . (cells Map.!) . Schema.columnName) cols

printTable :: TableSchema -> Table -> String
printTable schema (Table rows) = (unlines. map (printRow schema) . map snd . Map.toList) rows

