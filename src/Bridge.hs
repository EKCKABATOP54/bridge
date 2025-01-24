{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


module Bridge where

import Control.Monad.Except
import Control.Monad.Trans (liftIO)
import SimplexBotApi
import Simplex.Chat.Controller
import TelegramBot(TelegramAction, TelegramEvent(..), TelegramCommand(..))
import qualified Telegram.Bot.API.Types as TelegramAPI
import Control.Concurrent.STM
import Control.Monad
import qualified Database.SQLite.Simple as DB
import qualified Simplex.Messaging.Agent.Protocol as SMP (UserId, AConnectionRequestUri(..))
import Shared
import Control.Concurrent.MVar
import Puppet
import qualified DB.Puppet
import qualified DB.SimplexData
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Simplex.Chat.Types as SimplexTypes(User(..), Contact(..), localProfileId, LocalProfile(..), contactId', GroupInfo(..), GroupMember(..), Connection(..))
import qualified SimplexBotApi
import Simplex.Chat.Messages (AChatItem(..), ChatInfo(..), ChatItem(..), ChatType(..), ChatRef(..), CIDirection(..))
import Simplex.Chat.Messages.CIContent
import Simplex.Messaging.Encoding.String (StrEncoding(strDecode))
import qualified DB.Shared

data BridgeConfig = BridgeConfig{
    eventQueue :: TBQueue (Either ChatResponse TelegramEvent),
    chatController :: ChatController,
    telegramActionHandler :: TelegramAction -> IO (),
    bridgeDB :: DB.Connection,
    mainBotId :: MVar SMP.UserId,
    ownerInvatationLink :: MVar SMP.AConnectionRequestUri
}

mainBotFakePuppet = Puppet {tgUserId = TelegramAPI.UserId (-1), simplexUserId = -1}

runBrige :: BridgeConfig -> IO ()
runBrige bridgeConfig@BridgeConfig{chatController = cc, telegramActionHandler = tgActionHandler, bridgeDB = bridgedb, mainBotId = mainBotIdMVar, ownerInvatationLink = invatationLinkMVar} = forever $ do
    mainBotId' <- readMVar mainBotIdMVar
    event <- atomically $ readTBQueue (eventQueue bridgeConfig)
    r <- runExceptT (
        case event of
            Left simplexEvent -> processSimplexEvent simplexEvent mainBotId'
            Right telegramEvent -> processTelegramEvent telegramEvent mainBotId'
        )
    logEventProcessingError r
    where 
        processSimplexEvent event mainBotId = case event of
            CRContactConnected botacc@SimplexTypes.User{SimplexTypes.userId = puppetUserId} contact@SimplexTypes.Contact{profile = p} _ -> do
                _ownerInvatationLink <- liftIO $ tryReadMVar invatationLinkMVar
                
                when (puppetUserId == mainBotId) $ SimplexBotApi.setCCActiveUser cc mainBotId >>  SimplexBotApi.sendMessage cc contact welcomeMessage
                case _ownerInvatationLink of
                    Just (SMP.ACR _ cruri) -> do
                        --print (SimplexTypes.contactLink p)
                        --print cruri
                        when (maybe "" show (SimplexTypes.contactLink p) == show cruri) (do
                            p <- getPuppetBySimplexUser bridgedb cc botacc
                            liftIO $ putStrLn $ "owner contact : " ++ show (SimplexTypes.contactId' contact)
                            liftIO $ DB.Puppet.insertPuppetOwnerContactId bridgedb p (SimplexTypes.contactId' contact)
                            )
                    _ -> return ()
            -- direct chat
            CRNewChatItems {user = _user'@SimplexTypes.User{SimplexTypes.userId = puppetUserId}, chatItems = (AChatItem _ SMDRcv (DirectChat contact@SimplexTypes.Contact{contactId = cid}) ChatItem {content = mc@CIRcvMsgContent {}}) : _}
                | puppetUserId == mainBotId -> do
                    SimplexBotApi.setCCActiveUser cc mainBotId
                    case strDecode (Text.encodeUtf8 $ ciContentToText mc) of
                        Left error -> SimplexBotApi.sendMessage cc contact "This is not valid invatation link"
                        Right uri ->
                            (
                                do
                                    isFirstUser <- liftIO $ tryPutMVar invatationLinkMVar uri --TODO: it's not certain that if MVar is empty, tryPutMVar returns True
                                    if isFirstUser
                                        then (liftIO $ DB.Puppet.insertPuppetOwnerContactId bridgedb mainBotFakePuppet (SimplexTypes.contactId' contact)) >> (liftIO $ DB.SimplexData.insertOwnerInvatationLink bridgedb uri) >> SimplexBotApi.sendMessage cc contact "You are the owner now"
                                        else SimplexBotApi.sendMessage cc contact "Someone overtook you or you are already the owner" 
                            )
                | otherwise ->
                    (
                        do
                            puppet <- getPuppetBySimplexUser bridgedb cc _user'
                            mtgChatId <- liftIO $ DB.Puppet.getPuppetTgChat bridgedb puppet
                            liftIO $ maybe (putStrLn "Missing tg chat for puppet") (\tgChatId -> tgActionHandler $ Right $ MsgToChat tgChatId (ciContentToText mc)) mtgChatId
                    )
            --group chat
            CRNewChatItems {user = _user'@SimplexTypes.User{SimplexTypes.userId = puppetUserId}, chatItems = (AChatItem _ SMDRcv (GroupChat SimplexTypes.GroupInfo {groupId}) ChatItem {content = mc@CIRcvMsgContent {}, chatDir = chatDir}) : _} -> do
                mownerContactId <- liftIO $ DB.Puppet.getPuppetOwnerContactId bridgedb mainBotFakePuppet
                case (chatDir, mownerContactId) of
                    (CIGroupRcv SimplexTypes.GroupMember {memberContactId=mcId@(Just contactId)}, Just ownerContactId) -> liftIO $ when (puppetUserId == mainBotId && contactId == ownerContactId) (do
                        mtgChatId <- DB.Shared.getMainBotTgChatIdBySimplexGroupId bridgedb groupId
                        putStrLn $ "Get chat::::: " ++ (show mtgChatId)
                        case mtgChatId of
                            Just tgChatId -> tgActionHandler $ Right $ MsgToChat tgChatId (ciContentToText mc)
                            Nothing -> return ()
                        )
                    _ -> return ()

            CRGroupLinkConnecting {user = _user'@SimplexTypes.User{SimplexTypes.userId = puppetUserId}, hostMember = hostMember@SimplexTypes.GroupMember{groupId = groupId, activeConn = activeConn}} -> case activeConn of
                Just activeConn@SimplexTypes.Connection{connId = connId} -> do
                    mPuppetTgId <- liftIO $ DB.Shared.getPuppetTgChatIdByConnectionId bridgedb connId
                    case mPuppetTgId of
                        Just (puppetId, tgChatId) -> do
                            liftIO $ DB.Shared.insertPuppetGroupChat bridgedb puppetId (tgChatId, groupId)
                            liftIO $ putStrLn $ "connect conn tggroup simplexgroup" ++ show connId ++ " " ++ show tgChatId ++ " " ++ show groupId ++ " "
                        Nothing -> return ()
                Nothing -> return () 

            ev -> return ()
        processTelegramEvent event botId = case event of 
            MsgFromUser usr chat msg -> do
                liftIO $ putStrLn "new MsgFromUser"
                invatationLink' <- liftIO $ tryReadMVar invatationLinkMVar
                case invatationLink' of
                    Just invatationLink -> do
                        case TelegramAPI.chatType chat of
                            TelegramAPI.ChatTypePrivate -> processTelegramPrivateMessage cc bridgedb invatationLink usr chat msg
                            TelegramAPI.ChatTypeGroup -> processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId
                            TelegramAPI.ChatTypeSupergroup -> processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId
                            _ -> liftIO $ putStrLn "Unsupported chat type"
                    Nothing -> liftIO $ putStrLn "Missed invatation link. Cant process process telegram message"
        processTelegramPrivateMessage cc bridgedb invatationLink usr chat msg = do
            liftIO $ putStrLn $ "new private message"
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            liftIO $ DB.Puppet.insertPuppetTgChat bridgedb puppet (TelegramAPI.chatId chat)
            SimplexBotApi.setCCActiveUser cc (simplexUserId puppet)
            mchatId' <- liftIO $ DB.Puppet.getPuppetOwnerContactId bridgedb puppet
            liftIO $ putStrLn $ "Owner contact id: " ++ show mchatId'
            case mchatId' of
                Just chatId' -> SimplexBotApi.sendComposedMessage'' cc (ChatRef CTDirect chatId') Nothing (SimplexBotApi.textMsgContent' msg)
                Nothing -> liftIO $ putStrLn "Cant find interlocutor's contact"
        
        processTelegramGroupMessage cc bridgedb invatationLink usr chat msg botId= do
            liftIO $ putStrLn $ "New tg message from chat: " ++ (show chat)
            puppet <- getOrCreatePuppetByTgUser bridgedb cc usr invatationLink
            --SimplexBotApi.setCCActiveUser cc (simplexUserId puppet)
            mownerContactId <- liftIO $ DB.Puppet.getPuppetOwnerContactId bridgedb mainBotFakePuppet
            case mownerContactId of 
                Just ownerContactId -> do
                    mschatId <- getOrCreatePuppetSimplexGroupByTgChat ownerContactId botId puppet bridgedb cc chat
                    case mschatId of
                        Just schatId -> SimplexBotApi.setCCActiveUser cc (simplexUserId puppet) >> SimplexBotApi.sendGroupMessage cc schatId (Text.unpack msg)--SimplexBotApi.sendComposedMessage'' cc (ChatRef CTGroup gid) Nothing (SimplexBotApi.textMsgContent' msg)
                        Nothing -> liftIO $ putStrLn "Failed to get group chat"
                Nothing -> liftIO $ putStrLn "Cant get owner contact"
        logEventProcessingError e = case e of
            Left e -> print e
            Right () -> return ()
        

welcomeMessage :: String
welcomeMessage = "Send me your invatiation link. Puppets will use it to connect to you"
