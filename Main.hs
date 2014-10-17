module Main where

import Happstack.Server -- (nullConf, simpleHTTP, toResponse, ok, ServerPart, look)
import Control.Monad.Trans (liftIO)
import Database.SQLite
import Data.MarkovChain
import System.Random

-- STX and ETX stand for start and end of text. I want the algo to know where a phrase began. 
-- This is gonna be the extent of it while I use Data.MarkovChain, 
-- but once I make something fancier myself, I'l find something better.
getRelevantData :: Row String -> String
getRelevantData [(_, res)] = "\STX" ++ res ++ "\ETX"
getRelevantData _ = "ror"

extractFromDBOut :: [Row String] -> String
extractFromDBOut rows = foldl (\a b -> a ++ (getRelevantData b)) "" rows

-- that's it that's the thing
magic :: Int -> String -> String 
magic len input = take len $ run 2 input 0 (mkStdGen 123)


handleText :: String -> String -> IO String
handleText t w = do
    let entryLength = length t
    handle <- openConnection "db.sql"
    insert <- execParamStatement_ handle "INSERT INTO entries (what, who) VALUES (:to_insert, :who)" [(":to_insert", Text t), (":who", Text w)]  
    entries <- execParamStatement handle "SELECT what FROM entries WHERE who = :who" [(":who", Text w)] :: IO (Either String [[Row String]])
    case entries of 
        Left err -> return err
        Right [rows] -> return $ magic entryLength $ extractFromDBOut rows

-- execParamStatement_ :: SQLiteHandle -> String -> [(String, Value)] -> IO (Maybe String)
-- execStatement :: SQLiteResult a => SQLiteHandle -> String -> IO (Either String [[Row a]])

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

serverText :: ServerPart String
serverText = do
    decodeBody myPolicy
    input <- look "input"
    who <- look "who"
    handled <- liftIO $ handleText input who
    ok $ handled 

main :: IO ()
main = simpleHTTP nullConf $ serverText


