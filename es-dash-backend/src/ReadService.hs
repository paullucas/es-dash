{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReadService where

import           EventStoreSettings

import           Control.Monad
import           Database.EventStore
import qualified Data.Text as T
import qualified Network.WebSockets as WS

logEvt id =
  print $ T.append (T.pack "Event Appeared! StreamID: ") id

isNonStatEvt id =
  not (T.isPrefixOf "$" id)

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

main :: IO ()
main = do
  conn <- connect settings (Static "127.0.0.1" 1113)
  sub <- subscribeToAll conn False
  WS.runServer "127.0.0.1" 3000 $ readServerApp conn sub
  shutdown conn
  waitTillClosed conn
