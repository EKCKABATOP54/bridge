{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module SimplexBotApi
(
    initializeBotAddress,
    setCCActiveUser,
    createActiveUser,
    sendContactInvatation,
    sendMessage,
    sendComposedMessage'',
    sendGroupMessage,
    sendGroupMessage',
    textMsgContent,
    textMsgContent',
    createGroup,
    sendGroupInvatation,
    createGroupLink,
    connectToGroupByLink
    --getGroupInfo
)

where

import BM
import Control.Monad.Except
import Control.Monad
import Control.Monad.Trans (liftIO)
import Simplex.Chat.Controller
import Simplex.Chat.Core(sendChatCmd)
import Simplex.Chat.Types(Profile, Contact, ContactId, GroupProfile, GroupInfo, User, GroupId, ConnReqContact, pccConnId, contactId', NewUser(..))
import Simplex.Chat.Types.Shared(GroupMemberRole(..))
import Simplex.Chat.Messages(ChatItemId, ChatRef(..), ChatType(..))
import Data.List.NonEmpty
import Simplex.Chat.Store(UserContactLink (..), StoreError(..), AutoAccept(..))
import Simplex.Messaging.Agent.Protocol (AConnectionRequestUri (..), UserId)
import Simplex.Chat.Protocol(MsgContent (..))
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (exitFailure)

initializeBotAddress :: ChatController -> IO ()
initializeBotAddress = initializeBotAddress' True

initializeBotAddress' :: Bool -> ChatController -> IO ()
initializeBotAddress' logAddress cc = do
  sendChatCmd cc ShowMyAddress >>= \case
    CRUserContactLink _ UserContactLink {connReqContact} -> showBotAddress connReqContact
    CRChatCmdError _ (ChatErrorStore SEUserContactLinkNotFound) -> do
      when logAddress $ putStrLn "No bot address, creating..."
      sendChatCmd cc CreateMyAddress >>= \case
        CRUserContactLinkCreated _ uri -> showBotAddress uri
        _ -> putStrLn "can't create bot address" >> exitFailure
    _ -> putStrLn "unexpected response" >> exitFailure
  where
    showBotAddress uri = do
      when logAddress $ putStrLn $ "Bot's contact address is: " <> B.unpack (strEncode uri)
      void $ sendChatCmd cc $ AddressAutoAccept $ Just AutoAccept {acceptIncognito = False, autoReply = Nothing}

sendMessageErrorHandler :: ChatResponse -> BM ()
sendMessageErrorHandler r = case r of
  CRNewChatItems {} -> return ()
  e -> throwError $ UnexpectedChatResponse e

sendMessage :: ChatController -> Contact -> String -> BM ()
sendMessage cc ct = sendComposedMessage cc ct Nothing . textMsgContent

sendMessage' :: ChatController -> ContactId -> String -> BM ()
sendMessage' cc ctId = sendComposedMessage' cc ctId Nothing . textMsgContent

sendGroupMessage :: ChatController -> GroupId -> String -> BM ()
sendGroupMessage cc groupId msg = sendGroupMessage' cc groupId Nothing (textMsgContent msg)

sendGroupMessage' :: ChatController -> GroupId -> Maybe ChatItemId -> MsgContent -> BM ()
sendGroupMessage' cc groupId quotedItemId msgContent = do 
  let cm = ComposedMessage {fileSource = Nothing, quotedItemId, msgContent}
  liftIO (sendChatCmd cc (APISendMessages (ChatRef CTGroup groupId) False Nothing (cm :| []))) >>= sendMessageErrorHandler

sendComposedMessage :: ChatController -> Contact -> Maybe ChatItemId -> MsgContent -> BM ()
sendComposedMessage cc = sendComposedMessage' cc . contactId'

sendComposedMessage' :: ChatController -> ContactId -> Maybe ChatItemId -> MsgContent -> BM ()
sendComposedMessage' cc ctId quotedItemId msgContent = do
  let cm = ComposedMessage {fileSource = Nothing, quotedItemId, msgContent}
  liftIO (sendChatCmd cc (APISendMessages (ChatRef CTDirect ctId) False Nothing (cm :| []))) >>= sendMessageErrorHandler

sendComposedMessage'' :: ChatController -> ChatRef -> Maybe ChatItemId -> MsgContent -> BM ()
sendComposedMessage'' cc chatRef quotedItemId msgContent = do
  let cm = ComposedMessage {fileSource = Nothing, quotedItemId, msgContent}
  liftIO (sendChatCmd cc (APISendMessages chatRef False Nothing (cm :| []))) >>= sendMessageErrorHandler

sendContactInvatation :: ChatController -> AConnectionRequestUri ->  BM ()
sendContactInvatation cc invatationLink= do 
  let cmd = Connect False (pure invatationLink)
  liftIO (sendChatCmd cc cmd) >>= \case 
    CRSentInvitation {} -> return ()
    r -> throwError $ UnexpectedChatResponse r

contactRef :: Contact -> ChatRef
contactRef = ChatRef CTDirect . contactId'

textMsgContent :: String -> MsgContent
textMsgContent = MCText . T.pack

textMsgContent' :: T.Text -> MsgContent
textMsgContent' = MCText

createActiveUser :: ChatController -> Profile -> BM User
createActiveUser cc newUserProfile = do
  let profile = Just newUserProfile
  liftIO (sendChatCmd cc (CreateActiveUser NewUser {profile, pastTimestamp = False})) >>= \case
    CRActiveUser user -> return user
    r -> throwError $ UnexpectedChatResponse r

setCCActiveUser :: ChatController -> UserId -> BM ()
setCCActiveUser cc uid = do
  liftIO (sendChatCmd cc (APISetActiveUser uid Nothing)) >>= \case
    CRActiveUser _ -> return ()
    r -> throwError $ UnexpectedChatResponse r

getContactList :: ChatController -> BM [Contact]
getContactList cc = do
    liftIO (sendChatCmd cc ListContacts) >>= \case
      CRContactsList {contacts = contactList} -> return contactList
      r -> throwError $ UnexpectedChatResponse r

createGroup :: ChatController -> GroupProfile -> BM GroupInfo
createGroup cc groupProfile =
  liftIO (sendChatCmd cc (NewGroup False groupProfile)) >>= \case
    CRGroupCreated _ groupInfo -> return groupInfo
    r -> throwError $ UnexpectedChatResponse r

createGroupLink :: ChatController -> GroupId -> BM ConnReqContact
createGroupLink cc groupId = do
    liftIO (sendChatCmd cc (APICreateGroupLink groupId GRMember)) >>= \case
        CRGroupLinkCreated {connReqContact} -> return connReqContact
        r -> throwError $ UnexpectedChatResponse r

sendGroupInvatation :: ChatController -> GroupId -> ContactId -> BM ()
sendGroupInvatation cc groupId contactId = do
    liftIO $ print contactId
    liftIO (sendChatCmd cc (APIAddMember groupId contactId GRMember)) >>= \case
        CRSentGroupInvitation {} -> return ()
        r -> throwError $ UnexpectedChatResponse r

connectToGroupByLink :: ChatController -> AConnectionRequestUri -> BM Int64
connectToGroupByLink cc link = do
    liftIO (sendChatCmd cc (Connect False (Just link))) >>= \case
        CRSentInvitation {connection = c} -> return (pccConnId c)
        r -> throwError $ UnexpectedChatResponse r