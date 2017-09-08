{-# LANGUAGE ScopedTypeVariables #-}

module ReadService where

import           EventStoreSettings
import           Control.Monad
import           Database.EventStore
import qualified Data.Text as T
import qualified Network.WebSockets as WS

logEvt :: T.Text -> IO ()
logEvt id = print $ T.append (T.pack "Event Appeared! StreamID: ") id

isNonStatEvt :: T.Text -> Bool
isNonStatEvt id = not $ T.isPrefixOf (T.pack "$") id

sendEvt :: WS.Connection -> T.Text -> IO ()
sendEvt conn id = do
  WS.sendTextData conn id
  logEvt id

readLoop :: Subscription t => t -> WS.Connection -> IO b
readLoop gesSub wsConn = do
  event <- nextEvent gesSub
  let streamID :: T.Text = resolvedEventOriginalStreamId event
  when (isNonStatEvt streamID) $ sendEvt wsConn streamID
  readLoop gesSub wsConn

readServerApp :: Subscription t => t1 -> t -> WS.PendingConnection -> IO b
readServerApp gesConn gesSub pendingConn = readLoop gesSub =<< WS.acceptRequest pendingConn

main :: IO ()
main = do
  conn <- connect settings connectionType
  sub <- subscribeToAll conn False
  WS.runServer "127.0.0.1" 3000 $ readServerApp conn sub
  shutdown conn
  waitTillClosed conn
