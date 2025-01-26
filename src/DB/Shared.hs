{-# LANGUAGE OverloadedStrings #-}

module DB.Shared
(
    initGroupChatDB,
    insertPuppetGroupChat,
    getPuppetTgChatIdBySimplexGroupId,
    getPuppetGroupChatByTgChatId,

    initGroupLinksDB,
    insertGroupLink,
    getGroupLink,

    initPendingGroupConnectionsDB,
    insertPendingGroupConnection,
    getPuppetConnectionIdByTgChatId,
    getPuppetTgChatIdByConnectionId,

    initMainBotGroupIdDB,
    insertMainBotGroupId,
    getMainBotTgChatIdBySimplexGroupId
)

where

import Puppet
import DB.DBTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Simplex.Chat.Types(ConnReqContact)
import Simplex.Chat.Controller ( ChatController )
import qualified Telegram.Bot.API as TelegramAPI
import Telegram.Bot.API.Types.Common(ChatId(..))
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId, AConnectionRequestUri)
import Database.SQLite.Simple ( FromRow(..), NamedParam(..), Connection, Only(..), open, field, query, query_, executeNamed, execute_, queryNamed)
import Database.SQLite.Simple.FromRow (RowParser)
import Data.Int (Int64)
import Simplex.Messaging.Encoding.String
import Simplex.Chat.View

initGroupChatDB :: Connection -> IO ()
initGroupChatDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS groupChats (puppetId INTEGER, tgChatId INTEGER, simplexChatId INTEGER)"
    return ()
  
insertPuppetGroupChat :: Connection -> SMP.UserId -> (TelegramAPI.ChatId, Int64) -> IO ()
insertPuppetGroupChat conn puppetId (TelegramAPI.ChatId tgChat, sChat) = do
    executeNamed conn "INSERT INTO groupChats (puppetId, tgChatId, simplexChatId) VALUES (:puppet, :tgChat, :sChat)" [":puppet" := puppetId, ":tgChat" := tgChat, ":sChat" := sChat]


getPuppetGroupChatByTgChatId :: Connection -> Puppet -> TelegramAPI.ChatId -> IO (Maybe Int64)
getPuppetGroupChatByTgChatId conn puppet (TelegramAPI.ChatId tgChatId) = do
    simplexChatId' <- queryNamed conn "SELECT simplexChatId from groupChats WHERE puppetId = :puppet AND tgChatId = :tgChat" [":puppet" := simplexUserId puppet, ":tgChat" := (fromIntegral {--Dirty--} tgChatId :: Int64)] :: IO [RInt64]
    case simplexChatId' of
        [] -> return Nothing
        (simplexChatId:_) -> return $ Just $ getInt simplexChatId

getPuppetTgChatIdBySimplexGroupId :: Connection -> SMP.UserId -> Int64 -> IO (Maybe TelegramAPI.ChatId)
getPuppetTgChatIdBySimplexGroupId conn puppetId simplexChatId = do
    simplexChatId' <- queryNamed conn "SELECT tgChatId from groupChats WHERE puppetId = :puppetId AND simplexChatId = :simplexChatId" [":puppetId" := puppetId, ":simplexChatId" := simplexChatId] :: IO [RInt64]
    case simplexChatId' of
        [] -> return Nothing
        (simplexChatId:_) -> return $ Just $ TelegramAPI.ChatId $ toInteger $ getInt simplexChatId


initGroupLinksDB :: Connection -> IO ()
initGroupLinksDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS groupLinks (tgChatId INTEGER, simplexChatLink STRING)"
    return ()
  
insertGroupLink :: Connection -> ConnReqContact -> TelegramAPI.ChatId -> IO ()
insertGroupLink conn link (TelegramAPI.ChatId tgChat) = do
    executeNamed conn "INSERT INTO groupLinks (tgChatId, simplexChatLink) VALUES (:tgChat, :link)" [":tgChat" := tgChat, ":link" := Text.unpack (Text.decodeUtf8 $ strEncode (simplexChatContact link))]

getGroupLink :: Connection -> TelegramAPI.ChatId -> IO (Maybe SMP.AConnectionRequestUri) --(Maybe ConnReqContact) idk how to convert ConnectionRequestUri to AConnectionRequestUri. createGroupLink :: ChatController -> GroupId -> IO ConnReqContact but Connect IncognitoEnabled (Maybe AConnectionRequestUri)
getGroupLink conn  (TelegramAPI.ChatId tgChatId) = do
    simplexChatId' <- query conn "SELECT simplexChatLink from groupLinks WHERE tgChatId=?" (Only tgChatId) :: IO [RString]
    case simplexChatId' of
        [] -> return Nothing
        (simplexChatId:_) -> case strDecode $ Text.encodeUtf8 $ Text.pack $ getString simplexChatId of
          Left error -> return Nothing
          Right link -> return $ Just link

initPendingGroupConnectionsDB :: Connection -> IO ()
initPendingGroupConnectionsDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS pendingGroupConnections (puppetId INTEGER, tgChatId INTEGER, simplexConnectionId INTEGER)"
    return ()
  
insertPendingGroupConnection :: Connection -> Int64 -> TelegramAPI.ChatId -> Int64 -> IO ()
insertPendingGroupConnection conn puppetId (TelegramAPI.ChatId tgChatId) simplexConnectionId = do
    executeNamed conn "INSERT INTO pendingGroupConnections (puppetId, tgChatId, simplexConnectionId) VALUES (:puppetId, :tgChatId, :simplexConnectionId)" [":puppetId" := puppetId, ":tgChatId" := tgChatId, ":simplexConnectionId" := simplexConnectionId]
    return ()

getPuppetTgChatIdByConnectionId :: Connection -> Int64 -> IO (Maybe (SMP.UserId, TelegramAPI.ChatId))
getPuppetTgChatIdByConnectionId conn simplexConnectionId = do
    puppetId' <- queryNamed conn "SELECT puppetId from pendingGroupConnections WHERE simplexConnectionId = :simplexConnectionId" [":simplexConnectionId" := simplexConnectionId] :: IO [RSimplexUserID]
    tgChatId' <- queryNamed conn "SELECT tgChatId from pendingGroupConnections WHERE simplexConnectionId = :simplexConnectionId" [":simplexConnectionId" := simplexConnectionId] :: IO [RInt64]
    case (puppetId', tgChatId') of
        (puppetId : _ , tgChatId: _) -> return $ Just $ (getUID puppetId, TelegramAPI.ChatId $ toInteger $ getInt tgChatId)
        _ -> return Nothing

getPuppetConnectionIdByTgChatId :: Connection -> Int64 -> TelegramAPI.ChatId -> IO (Maybe Int64)
getPuppetConnectionIdByTgChatId conn puppetId (TelegramAPI.ChatId tgChatId) = do
    tgChatId' <- queryNamed conn "SELECT simplexConnectionId from pendingGroupConnections WHERE tgChatId = :tgChatId AND puppetId = :puppetId" [":tgChatId" := tgChatId, ":puppetId" := puppetId] :: IO [RInt64]
    case tgChatId' of
        [] -> return Nothing
        (tgChatId:_) -> return $ Just $ getInt tgChatId


initMainBotGroupIdDB :: Connection -> IO ()
initMainBotGroupIdDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS groupIdMainBot (tgChatId INTEGER, simplexChatId STRING)"
    return ()
  
insertMainBotGroupId :: Connection -> TelegramAPI.ChatId -> Int64 -> IO ()
insertMainBotGroupId conn (TelegramAPI.ChatId tgChatId) simplexChatId = do
    executeNamed conn "INSERT INTO groupIdMainBot (tgChatId, simplexChatId) VALUES (:tgChatId, :simplexChatId)" [":tgChatId" := tgChatId, ":simplexChatId" := simplexChatId]

getMainBotTgChatIdBySimplexGroupId :: Connection -> Int64-> IO (Maybe TelegramAPI.ChatId)
getMainBotTgChatIdBySimplexGroupId conn simplexChatId = do
    tgChatId' <- query conn "SELECT tgChatId from groupIdMainBot WHERE simplexChatId=?" (Only simplexChatId) :: IO [RInt64]
    case tgChatId' of
        [] -> return Nothing
        (tgChatId:_) -> return $ Just $ TelegramAPI.ChatId $ toInteger $ getInt tgChatId
