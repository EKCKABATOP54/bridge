{-# LANGUAGE OverloadedStrings #-}

module DB.Puppet
(
    initPuppetDB,
    selectPuppeteerUserId, 
    insertPuppet, 
    getPuppetByTgId,
    getPuppetBySimplexId,
    initPuppetTgChatDB,
    insertPuppetTgChat,
    getPuppetTgChat,
    initPuppetOwnerContactIdDB,
    insertPuppetOwnerContactId,
    getPuppetOwnerContactId
)

where

import Puppet
import qualified Telegram.Bot.API as TelegramAPI
import Telegram.Bot.API.Types.Common(ChatId(..))
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId)
import Database.SQLite.Simple ( FromRow(..), NamedParam(..), Connection, Only(..), field, query, query_, executeNamed, execute_)
import Database.SQLite.Simple.FromRow (RowParser)
import Data.Int (Int64)
import DB.DBTypes(RSimplexUserID(..), RInt64(..))

instance FromRow Puppet where
    fromRow = (\ _ _ sid tguid-> Puppet {tgUserId = TelegramAPI.UserId tguid, simplexUserId = sid}) <$> (field::RowParser Integer) <*> (field::RowParser Integer) <*> field <*> field

initPuppetDB :: Connection -> IO ()
initPuppetDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    execute_ conn "CREATE TABLE IF NOT EXISTS botPuppets (id INTEGER PRIMARY KEY, isPuppeteer BOOLEAN, simplexUserID INTEGER, telegramUserID INTEGER)"
    return ()

selectPuppeteerUserId :: Connection -> IO (Maybe SMP.UserId)
selectPuppeteerUserId conn = do
    r <- query_ conn "SELECT simplexUserID from botPuppets WHERE isPuppeteer=TRUE" :: IO [RSimplexUserID]
    case r of 
        [] -> return Nothing
        _ -> return $ Just $ getUID $ head r

insertPuppet :: Connection -> Bool -> Puppet -> IO ()
insertPuppet conn isPuppeter puppet = do
    if isPuppeter then execute_ conn "UPDATE botPuppets SET isPuppeteer = FALSE" else return ()
    executeNamed conn "INSERT INTO botPuppets (isPuppeteer, simplexUserID, telegramUserID) VALUES (:pflag,:suid, :tguid)" [":pflag" := (isPuppeterToInt isPuppeter :: Int), ":suid" := simplexUserId puppet, ":tguid" := tgid]
    where
        isPuppeterToInt p = if p then 1 else 0
        TelegramAPI.UserId tgid = tgUserId puppet

getPuppetByTgId :: Connection -> TelegramAPI.UserId -> IO (Maybe Puppet)
getPuppetByTgId conn (TelegramAPI.UserId tgUserId) = do
    puppet' <- query conn "SELECT * from botPuppets WHERE telegramUserID=? " (Only(fromIntegral {--Dirty--} tgUserId :: Int64)) :: IO [Puppet]
    case puppet' of
        [] -> return Nothing
        (p:_) -> return $ Just p 

getPuppetBySimplexId :: Connection -> SMP.UserId -> IO (Maybe Puppet)
getPuppetBySimplexId conn simplexUserId = do
    puppet' <- query conn "SELECT * from botPuppets WHERE simplexUserID=? " (Only(fromIntegral {--Dirty--} simplexUserId :: Int64)) :: IO [Puppet]
    case puppet' of
        [] -> return Nothing
        (p:_) -> return $ Just p


initPuppetTgChatDB :: Connection -> IO ()
initPuppetTgChatDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    execute_ conn "CREATE TABLE IF NOT EXISTS puppetTgChat (telegramUserID INTEGER, telegramChatID INTEGER)"
    return ()

insertPuppetTgChat :: Connection -> Puppet -> Telegram.Bot.API.Types.Common.ChatId -> IO()
insertPuppetTgChat conn puppet (Telegram.Bot.API.Types.Common.ChatId tgchatid) = do
    -- TODO: don't insert duplicates
    executeNamed conn "INSERT INTO puppetTgChat (telegramUserID, telegramChatID) VALUES (:tguid, :tgchatid)" [":tguid" := tgid, ":tgchatid" := tgchatid]
    where
        TelegramAPI.UserId tgid = tgUserId puppet

getPuppetTgChat :: Connection -> Puppet -> IO (Maybe Telegram.Bot.API.Types.Common.ChatId)
getPuppetTgChat conn puppet = do
  chats <- query conn "SELECT telegramChatID from puppetTgChat WHERE telegramUserID=?" (Only tgid) :: IO [RInt64]
  case chats of
    chat:_ -> (return . Just . Telegram.Bot.API.Types.Common.ChatId . toInteger . getInt) chat
    _ -> return Nothing
    where
        TelegramAPI.UserId tgid = tgUserId puppet


initPuppetOwnerContactIdDB :: Connection -> IO ()
initPuppetOwnerContactIdDB conn = do
    -- TODO: better table structure
    -- maybe its possible to assume that puppeter always has id=1, but i dont shure how sqlite works
    execute_ conn "CREATE TABLE IF NOT EXISTS puppetOwnerContactId (puppetSimpexId INTEGER, contactId INTEGER)"
    return ()

insertPuppetOwnerContactId :: Connection -> Puppet -> Int64 -> IO()
insertPuppetOwnerContactId conn puppet contactId = do
    -- TODO: don't insert duplicates
    executeNamed conn "INSERT INTO puppetOwnerContactId (puppetSimpexId, contactId) VALUES (:sid, :cid)" [":sid" := simplexUserId puppet, ":cid" := contactId]

getPuppetOwnerContactId :: Connection -> Puppet -> IO (Maybe Int64)
getPuppetOwnerContactId conn puppet = do
  ids <- query conn "SELECT contactId from puppetOwnerContactId WHERE puppetSimpexId=?" (Only $ simplexUserId puppet) :: IO [RInt64]
  case ids of
    id':_ -> return $ Just $ getInt id'
    _ -> return Nothing
