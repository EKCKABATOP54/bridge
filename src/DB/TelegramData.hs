{-# LANGUAGE OverloadedStrings #-}

module DB.TelegramData
(
    initTelegramTokenDB,
    insertTelegramToken,
    getTelegramToken
)
where

import DB.DBTypes
import Database.SQLite.Simple (Connection, Only(..), query_, execute_, execute)
--import Database.SQLite.Simple.FromRow (RowParser)


initTelegramTokenDB :: Connection -> IO ()
initTelegramTokenDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS telegramToken (token TEXT)"
    return ()

insertTelegramToken :: Connection -> String -> IO()
insertTelegramToken conn link = do
    -- TODO: don't insert duplicates
    execute conn "INSERT INTO telegramToken (token) VALUES (?)" (Only link)

getTelegramToken :: Connection -> IO (Maybe String)
getTelegramToken conn = do
  tokens <- query_ conn "SELECT * from telegramToken":: IO [RString]
  case tokens of
    token:_ -> return $ Just (getString token)
    _ -> return Nothing