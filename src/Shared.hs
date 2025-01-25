{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared
(
    getOrCreatePuppetByTgUser,
    getPuppetBySimplexUser,
    getOrCreatePuppetSimplexGroupByTgChat
    --initGroupLinksDB
) 
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe
import Control.Monad.Reader
import Puppet
import Simplex.Chat.Controller
import Simplex.Messaging.Agent.Protocol(AConnectionRequestUri)
import Simplex.Chat.Types -- (Profile(Profile), User, userId)
import Database.SQLite.Simple as DB(Connection)
import Telegram.Bot.API.Types.Common(ChatId(..))
import qualified Telegram.Bot.API.Types.Chat as TelegramApi
import qualified Telegram.Bot.API.Types.User as TgUser
import DB.DBTypes
import qualified DB.Puppet
import qualified DB.Shared
import qualified SimplexBotApi
import BM
import Data.Int (Int64)
import qualified Simplex.Messaging.Agent.Protocol as SMP (UserId)
import Simplex.Messaging.Encoding.String

getOrCreatePuppetByTgUser :: DB.Connection -> ChatController -> TgUser.User -> AConnectionRequestUri -> BM Puppet
getOrCreatePuppetByTgUser conn cc tguser invatationLink = do
  let tgUserId' = TgUser.userId tguser
  puppet' <- liftIO $ DB.Puppet.getPuppetByTgId conn tgUserId'
  case puppet' of
    Just p -> do
      return p
    Nothing -> do
      let displayName = Text.concat [(TgUser.userFirstName tguser), Text.pack " ", (fromMaybe "" (TgUser.userLastName tguser))]
      correspondingSimplexUser@Simplex.Chat.Types.User{userId = simplexUserId'} <- SimplexBotApi.createActiveUser cc (Profile {displayName, fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}) 
      let puppet = Puppet {tgUserId = tgUserId', simplexUserId = simplexUserId'}
      liftIO $ DB.Puppet.insertPuppet conn False puppet
      SimplexBotApi.sendContactInvatation cc invatationLink
      return puppet


getPuppetBySimplexUser :: DB.Connection -> ChatController -> Simplex.Chat.Types.User -> BM Puppet
getPuppetBySimplexUser conn cc simplexUser@Simplex.Chat.Types.User{userId=simplexUserId} = do 
    puppet' <- liftIO $ DB.Puppet.getPuppetBySimplexId conn simplexUserId
    case puppet' of
        Just p -> return p
        Nothing -> fail "No simplex user for this id"

getOrCreatePuppetSimplexGroupByTgChat :: Int64 -> SMP.UserId -> Puppet -> DB.Connection -> ChatController -> TelegramApi.Chat -> BM (Maybe GroupId)
getOrCreatePuppetSimplexGroupByTgChat ownerMainBotContactId mainBotId puppet conn cc chat = do
  sChatId' <- liftIO $ DB.Shared.getPuppetGroupChatByTgChatId conn puppet (TelegramApi.chatId chat)
  case sChatId' of
    Just sChatId -> return $ Just sChatId
    Nothing -> do
      mlink <- liftIO $ DB.Shared.getGroupLink conn (TelegramApi.chatId chat)
      case mlink of
        Just link -> connectPuppetToGroup conn cc puppet link (TelegramApi.chatId chat) >> return Nothing
        Nothing -> do
          SimplexBotApi.setCCActiveUser cc mainBotId  -- create group on behalf of main bot
          groupInfo@GroupInfo {groupId = gId} <- SimplexBotApi.createGroup cc (
            GroupProfile {
                displayName = fromMaybe (Text.pack $ show $ TelegramApi.chatId chat) (TelegramApi.chatTitle chat),
                fullName = "",
                description = Nothing,
                image = Nothing,
                groupPreferences = Nothing
                })
          liftIO $ DB.Shared.insertMainBotGroupId conn (TelegramApi.chatId chat) gId
          SimplexBotApi.sendGroupInvatation cc gId ownerMainBotContactId
          groupInvatationLink <- SimplexBotApi.createGroupLink cc gId
          liftIO $ DB.Shared.insertGroupLink conn groupInvatationLink (TelegramApi.chatId chat)
          --idk how to better conver ConnReqContact to AConnectionRequestUri 
          connectPuppetToGroup conn cc puppet (case strDecode $ Text.encodeUtf8 (Text.decodeUtf8 $ strEncode groupInvatationLink) of Right l -> l) (TelegramApi.chatId chat)
          return Nothing
  where
    connectPuppetToGroup conn cc (Puppet {simplexUserId = puppetUserId}) groupInvatationLink tgChatId = do
      SimplexBotApi.setCCActiveUser cc puppetUserId
      pendingConnId <- liftIO $ DB.Shared.getConnectionIdByTgChatId conn tgChatId
      case pendingConnId of
        Just _ -> return () -- already trying to connect to group
        Nothing -> do
          pendingConnId <- SimplexBotApi.connectToGroupByLink cc groupInvatationLink
          liftIO $ DB.Shared.insertPendingGroupConnection conn puppetUserId tgChatId pendingConnId
          return ()