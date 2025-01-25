
module Puppet(
    Puppet(..)
)

where

import qualified Telegram.Bot.API as TelegramAPI(UserId)
import qualified Simplex.Messaging.Agent.Protocol as SMP(UserId)
import DB.DBTypes

data Puppet = Puppet {
    tgUserId :: TelegramAPI.UserId,
    simplexUserId :: SMP.UserId
} deriving Show
