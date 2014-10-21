module Main where

import Happstack.Server -- (nullConf, simpleHTTP, toResponse, ok, ServerPart, look)
import Control.Monad.Trans (liftIO)
import Database.SQLite
import Data.MarkovChain
import System.Random
import Control.Monad.Reader

-- type DbMonad a = ReaderT SQLiteHandle ServerPart a
type DbMonad = ReaderT SQLiteHandle (ServerPartT IO)

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


handleText :: SQLiteHandle -> String -> String -> IO String
handleText handle t w = do
    let entryLength = length t
    -- handle <- openConnection "db.sql"
    insert <- execParamStatement_ handle "INSERT INTO entries (what, who) VALUES (:to_insert, :who)" [(":to_insert", Text t), (":who", Text w)]  
    entries <- execParamStatement handle "SELECT what FROM entries WHERE who = :who" [(":who", Text w)] :: IO (Either String [[Row String]])
    case entries of 
        Left err -> return err
        Right [rows] -> return $ magic entryLength $ extractFromDBOut rows


myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

serverText :: DbMonad String
serverText = do
    decodeBody myPolicy
    input <- look "input"
    who <- look "who"
    handle <- ask
    handled <- liftIO $ handleText handle input who
    ok $ handled 

runDbMonad :: DbMonad a -> ServerPart a
runDbMonad m = do
    db <- liftIO $ openConnection "db.sql"
    runReaderT m db

main :: IO ()
main = simpleHTTP nullConf $ runDbMonad serverText 
