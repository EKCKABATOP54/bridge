module BM
(
    BotError(..),
    BM   
)
where

import Control.Monad.Except
import Control.Monad
import Simplex.Chat.Controller(ChatResponse)

import Puppet
import Telegram.Bot.API.Types.ChatType (ChatType)

data BotError = 
    UnexpectedChatResponse ChatResponse 
    | MissedPuppetOwnerContactId Puppet
    | UnsupportedTelegramChatType ChatType
    deriving Show


type BM a = ExceptT BotError IO a

