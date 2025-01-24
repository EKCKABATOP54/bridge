module BM
(
    BotError(..),
    BM   
)
where

import Control.Monad.Except
import Control.Monad
import Simplex.Chat.Controller(ChatResponse)


newtype BotError = UnexpectedChatResponse ChatResponse deriving Show


type BM a = ExceptT BotError IO a

