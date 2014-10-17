module Main where

import Happstack.Server -- (nullConf, simpleHTTP, toResponse, ok, ServerPart, look)
import Control.Monad.Trans (liftIO)
import Database.SQLite

getRelevantData :: Row String -> String
getRelevantData [(_, res)] = res
getRelevantData _ = "ror"

handleText' :: String -> String -> IO String
handleText' t w = do
    handle <- openConnection "db.sql"
    insert <- execParamStatement_ handle "INSERT INTO entries (what, who) VALUES (:to_insert, :who)" [(":to_insert", Text t), (":who", Text w)]  
    -- entries <- execStatement handle "SELECT what FROM entries" :: IO (Either String [[Row String]])
    entries <- execParamStatement handle "SELECT what FROM entries WHERE who = :who" [(":who", Text w)] :: IO (Either String [[Row String]])
    case entries of 
        Left err -> return err
        Right [rows] -> return $ foldl (\a b -> a ++ (getRelevantData b)) "" rows

-- execParamStatement_ :: SQLiteHandle -> String -> [(String, Value)] -> IO (Maybe String)
-- execStatement :: SQLiteResult a => SQLiteHandle -> String -> IO (Either String [[Row a]])

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handleText :: String -> IO String
handleText t = do
    putStrLn t
    return "yeh"

serverText :: ServerPart String
serverText = do
    decodeBody myPolicy
    input <- look "input"
    who <- look "who"
    handled <- liftIO $ handleText' input who
    ok $ handled ++ ", aight?"

main :: IO ()
main = simpleHTTP nullConf $ serverText


