{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WriteService where

import           User
import           EventStoreSettings

import           Control.Concurrent.Async (Async)
import           Database.EventStore
import qualified Data.Text as T
import           Data.UUID (toString, toText)
import           Data.UUID.V4 (nextRandom)
import qualified Network.WebSockets as WS

createUser :: Connection -> T.Text -> IO (Async WriteResult)
createUser conn username = do
  uuid <- nextRandom
  let user = withJson $ User { userID = (toString uuid), name = (T.unpack username) }
      stream = StreamName $ T.append (T.pack "user-") (toText uuid)
      event  = createEvent "userCreated" Nothing user
  sendEvent conn stream anyVersion event

writeLoop gesConn wsConn = do
  msg :: T.Text <- WS.receiveData wsConn
  print $ "Creating user: " ++ (T.unpack msg)
  createUser gesConn msg
  writeLoop gesConn wsConn

writeServerApp gesConn pendingConn =
  writeLoop gesConn =<< WS.acceptRequest pendingConn

main :: IO ()
main = do
  conn <- connect settings connectionType
  WS.runServer "127.0.0.1" 3001 $ writeServerApp conn
  shutdown conn
  waitTillClosed conn
