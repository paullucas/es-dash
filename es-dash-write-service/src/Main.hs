{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import qualified Data.Text as T
import           Database.EventStore
import qualified Network.WebSockets as WS
import           Data.Aeson
import           GHC.Generics
import           Data.UUID (toString, toText)
import           Data.UUID.V4 (nextRandom)
import           Control.Concurrent.Async (wait, Async)


data User = User
  { userID :: String
  , name :: String
  } deriving (Generic, Show)


instance ToJSON User
instance FromJSON User


createUser :: Connection -> T.Text -> IO (Async WriteResult)
createUser conn username = do
  uuid <- nextRandom
  let user = withJson $ User { userID = (toString uuid), name = (T.unpack username) }
      stream = StreamName $ T.append (T.pack "user-") (toText uuid)
      event  = createEvent "userCreated" Nothing user
      --     event  = createEvent "userCreated" Nothing user
  sendEvent conn stream anyVersion event
  -- sendEvent conn "Users" anyVersion event  


parseMember :: ResolvedEvent -> Maybe User
parseMember = resolvedEventDataAsJson


settings :: Settings
settings = defaultSettings
  { s_credentials = Just (credentials "admin" "changeit") }


loop gesConn wsConn = do
  msg :: T.Text <- WS.receiveData wsConn
  print $ "event! " ++ (T.unpack msg)
  createUser gesConn msg
  loop gesConn wsConn


serverApp gesConn pendingConn = do
    wsConn <- WS.acceptRequest pendingConn
    loop gesConn wsConn


main :: IO ()
main = do
  conn <- connect settings (Static "127.0.0.1" 1113)
  WS.runServer "127.0.0.1" 3001 $ serverApp conn
  shutdown conn
  waitTillClosed conn
