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
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (wait, Async)

-- User

data User = User { userID :: String, name :: String } deriving (Generic, Show)
instance ToJSON User
instance FromJSON User

createUser :: Connection -> T.Text -> IO (Async WriteResult)
createUser conn username = do
  uuid <- nextRandom
  let user = withJson $ User { userID = (toString uuid), name = (T.unpack username) }
      stream = StreamName $ T.append (T.pack "user-") (toText uuid)
      event  = createEvent "userCreated" Nothing user
  sendEvent conn stream anyVersion event

parseMember :: ResolvedEvent -> Maybe User
parseMember = resolvedEventDataAsJson

-- Read

logEvt id =
  print $ T.append (T.pack "Event Appeared! StreamID: ") id

isNonStatEvt id =
  not $ T.isPrefixOf "$" id

sendEvt conn id = do
  WS.sendTextData conn id
  logEvt id

readLoop gesSub wsConn = do
  event <- nextEvent gesSub
  let streamID :: T.Text = resolvedEventOriginalStreamId event
  when (isNonStatEvt streamID) $ sendEvt wsConn streamID
  readLoop gesSub wsConn

readServerApp gesConn gesSub pendingConn =
  readLoop gesSub =<< WS.acceptRequest pendingConn

-- Write

writeLoop gesConn wsConn = do
  msg :: T.Text <- WS.receiveData wsConn
  print $ "Creating user: " ++ (T.unpack msg)
  createUser gesConn msg
  writeLoop gesConn wsConn

writeServerApp gesConn pendingConn =
  writeLoop gesConn =<< WS.acceptRequest pendingConn

-- Main

settings :: Settings
settings = defaultSettings
  { s_credentials = Just (credentials "admin" "changeit") }

main :: IO ()
main = do
  conn <- connect settings (Static "127.0.0.1" 1113)
  sub <- subscribeToAll conn False
  forkIO $ WS.runServer "127.0.0.1" 3000 $ readServerApp conn sub
  WS.runServer "127.0.0.1" 3001 $ writeServerApp conn
  shutdown conn
  waitTillClosed conn
