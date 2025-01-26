{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SimplexBot
(
    initializeSimplexChatCore,
    mySimplexBot
)
where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Reader
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore, withTransaction)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async
import qualified Database.SQLite.Simple as DB
import Data.List
import Control.Concurrent (forkIO)
import Data.Int(Int64)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Telegram.Bot.API as TelegramAPI
import Control.Concurrent.MVar
import Simplex.Messaging.Agent.Protocol -- (UserId, AConnectionRequestUri(..), UserId)
--import qualified Simplex.Chat.Bot as SimplexChatBotApi
import SimplexBotApi
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.View (serializeChatResponse)
import Simplex.Chat.Messages
import UnliftIO.Async
import Control.Concurrent.STM
import Simplex.Messaging.Encoding.String (StrEncoding(strDecode))
import TelegramBot (TelegramEvent(..))
import Telegram.Bot.API.Types.Common(ChatId(..))
import Puppet
import DB.Puppet

initializeSimplexChatCore :: ChatConfig -> ChatOpts -> DB.Connection ->  (User -> ChatController -> IO ()) -> IO ChatController
initializeSimplexChatCore cfg@ChatConfig {confirmMigrations, testView} opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix, dbKey, logAgent}} botDB chat =
  case logAgent of
    Just level -> do
      setLogLevel level
      withGlobalLogging logCfg initRun
    _ -> initRun
  where
    initRun = createChatDatabase dbFilePrefix dbKey False confirmMigrations >>= either exit run
    exit e = do
      putStrLn $ "Error opening database: " <> show e
      exitFailure
    run db@ChatDatabase {chatStore} = do
      u_ <- getSelectActiveUser chatStore botDB
      cc <- newChatController db u_ cfg opts False
      u <- createMainBotIfMissed cc u_
      unless testView $ putStrLn $ "Current user: " <> userStr u
      forkIO (runSimplexChat opts u cc chat)
      return cc
    createMainBotIfMissed cc mu = case mu of
                                        Just u -> pure u
                                        Nothing -> do
                                                u@User{userId} <- createMainBot cc
                                                DB.Puppet.insertPuppet botDB True (Puppet {tgUserId = TelegramAPI.UserId 0, simplexUserId = userId})
                                                return u

runSimplexChat :: ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatOpts {maintenance} u cc chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
      a1 <- runReaderT (startChatController True True) cc
      a2 <- async $ chat u cc
      waitEither_ a1 a2

getSelectActiveUser :: SQLiteStore -> DB.Connection -> IO (Maybe User)
getSelectActiveUser st botDB = do
  users <- withTransaction st getUsers
  selectUser users botDB
  where
    selectUser :: [User] -> DB.Connection -> IO (Maybe User)
    selectUser users botDB = do
        puppeterUserId <- selectPuppeteerUserId botDB
        return $ puppeterUserId >>= (\uid -> find (\u@User{userId} -> userId == uid) users)

createMainBot :: ChatController -> IO User
createMainBot cc = do
  putStrLn
    "No bot profiles found, it will be created now.\n\
    \Please choose your bot name.\n\
    \It will be sent to your contacts when you connect.\n\
    \It is only stored on your device and you can change it later."
  loop
  where
    loop = do
      displayName <- Text.pack <$> getWithPrompt "bot display name"
      let profile = Just Profile {displayName, fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}
      execChatCommand' (Simplex.Chat.Controller.CreateActiveUser NewUser {profile, pastTimestamp = False}) `runReaderT` cc >>= \case
        CRActiveUser user -> pure user
        r -> do
          ts <- getCurrentTime
          tz <- getCurrentTimeZone
          putStrLn $ serializeChatResponse (Nothing, Nothing) ts tz Nothing r
          loop

getWithPrompt :: String -> IO String
getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

userStr :: User -> String
userStr User {localDisplayName, profile = LocalProfile {fullName}} =
  Text.unpack $ localDisplayName <> if Text.null fullName || localDisplayName == fullName then "" else " (" <> fullName <> ")"

mySimplexBot :: MVar UserId -> DB.Connection -> TBQueue (Either ChatResponse TelegramEvent) -> User -> ChatController -> IO ()
mySimplexBot puppeterIdMVar conn eventQueue _user@User{userId = _userId} cc = do
  putMVar puppeterIdMVar _userId
  SimplexBotApi.initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, _, resp) <- atomically . readTBQueue $ outputQ cc
    atomically $ writeTBQueue eventQueue (Left resp)

welcomeMessage :: String
welcomeMessage = "Send me your invatiation link. Puppets will use it to connect to you"
