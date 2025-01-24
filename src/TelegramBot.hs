{-# LANGUAGE FlexibleContexts #-}

module TelegramBot
(
    Model(..),
    TelegramEvent(..),
    TelegramCommand(..),
    TelegramAction,
    updateToAction, 
    handleTgAction
)

where 

import qualified Data.Text as Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText, updateMessageSticker)
import Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import qualified Data.Text.Internal.Read as Text
import Control.Monad.Reader
import Control.Concurrent.STM
import Simplex.Chat.Controller(ChatResponse)
import Telegram.Bot.API.Types.Common(ChatId(..), SomeChatId(..))
import Telegram.Bot.API.Types.User

type Model = ()

data TelegramEvent =
    MsgFromUser User Chat Text.Text

data TelegramCommand =
    MsgToChat ChatId Text.Text

type TelegramAction = Either TelegramEvent TelegramCommand

updateToAction :: Update -> Model -> Maybe TelegramAction
updateToAction update _ = do 
    msg <- updateMessage update
    usr <- messageFrom msg
    text <- messageText msg
    return $ Left $ MsgFromUser usr (messageChat msg) text

handleTgAction :: TBQueue (Either ChatResponse TelegramEvent) -> TelegramAction -> Model -> Eff TelegramAction Model
handleTgAction eventQueue action model = case action of
    Left e -> processTelegramEvent e
    Right c -> processTelegramCommand c
    where
        putEvent a = liftIO $ atomically $ writeTBQueue eventQueue (Right a)

        processTelegramEvent e = model <# do putEvent e

        processTelegramCommand command = case command of
            MsgToChat c t -> model <# do
                let sendMsgReq = SendMessageRequest{
                    sendMessageBusinessConnectionId = Nothing,
                    sendMessageChatId = SomeChatId c,
                    sendMessageMessageThreadId = Nothing,
                    sendMessageText = t,
                    sendMessageParseMode = Nothing,
                    sendMessageEntities = Nothing,
                    sendMessageLinkPreviewOptions = Nothing,
                    sendMessageDisableNotification = Nothing,
                    sendMessageProtectContent = Nothing,
                    sendMessageMessageEffectId = Nothing,
                    sendMessageReplyToMessageId = Nothing,
                    sendMessageReplyParameters = Nothing,
                    sendMessageReplyMarkup = Nothing
                }
                _ <- runTG $ sendMessage sendMsgReq
                return ()
