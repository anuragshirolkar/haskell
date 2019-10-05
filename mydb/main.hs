import qualified System.IO as IO
import qualified Schema as Schema
import DB (Row, Cell, Table)
import qualified DB as DB
import qualified Connection as Connection

main :: IO ()
main =
    do
        conn <- Connection.connectToDb "test.db"
        --userIdStr <- getLine
        --rows <- Connection.query conn userIdStr
        --putStrLn (show rows)
        upsertRow <- getLine
        ok <- Connection.upsert conn upsertRow
        putStrLn (show ok)
        return ()
