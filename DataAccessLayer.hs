module DataAccessLayer where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parser


-- Initialize DB and return database Connection
connect :: IO Connection
connect =
    do conection <- connectSqlite3 "Project.db"       
       return conection

createDatabase :: IO ()
createDatabase  = do  conn <- connect
		      createSchema conn
              	      commit conn

createSchema :: IConnection conn => conn -> IO ()
createSchema con =
    do     run con "CREATE TABLE IF NOT EXISTS Images(pageUrl TEXT, imgPath TEXT)" []
           run con "CREATE TABLE IF NOT EXISTS Links (pageUrl TEXT, linkPath TEXT)" []
       	   commit con

saveImage :: [PageImage] -> IO ()
saveImage [] = return ()
saveImage pageImgs =
     do   conn <- connect
          stmt <- prepare conn "INSERT INTO Images (pageUrl, imgPath) VALUES (?, ?)"
          executeMany stmt (map (\pm -> [toSql (page pm), toSql (img pm)]) pageImgs)
          commit conn


saveLinks :: [PageLink] -> IO ()
saveLinks [] = return ()
saveLinks pageLinks =
     do conn <- connect
        stmt <- prepare conn "INSERT INTO Links (pageUrl, linkPath) VALUES (?, ?)"
        executeMany stmt (map (\pl -> [toSql (pageUrl pl), toSql (link pl)]) pageLinks)
        commit conn

loadImages :: IO ()
loadImages = do  conn <- connect
                 resust <- quickQuery conn "SELECT * FROM Images" []
                 mapM_ putStrLn (map convRow resust)
	         disconnect conn



loadLinks :: IO ()
loadLinks = do   conn <- connect
                 resust <- quickQuery' conn "SELECT * FROM Links" []
                 mapM_ putStrLn (map convRow resust)
	         disconnect conn


convRow :: [SqlValue] -> String
convRow [sqlCol1, sqlCol2] = show desc1 ++ " || " ++ desc2
		where desc1 = case fromSql sqlCol1 of
			Just x -> x
			Nothing -> "NULL"
		      desc2 = case fromSql sqlCol2 of
			Just x -> x
			Nothing -> "NULL"

