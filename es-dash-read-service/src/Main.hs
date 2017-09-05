{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.Text as T
import           Database.EventStore
import qualified Network.WebSockets as WS


settings :: Settings
settings = defaultSettings
  { s_credentials = Just (credentials "admin" "changeit") }


loop gesSub wsConn = do
  event <- nextEvent gesSub
  let msg = T.append (T.pack "Event Appeared! StreamID: ") $ resolvedEventOriginalStreamId event
  print msg
  WS.sendTextData wsConn msg
  loop gesSub wsConn


serverApp :: Subscription t => t1 -> t -> WS.PendingConnection -> IO b
serverApp gesConn gesSub pendingConn = do
  wsConn <- WS.acceptRequest pendingConn
  loop gesSub wsConn


main :: IO ()
main = do
  conn <- connect settings (Static "127.0.0.1" 1113)
  sub <- subscribeToAll conn False
  WS.runServer "127.0.0.1" 3000 $ serverApp conn sub
  shutdown conn
  waitTillClosed conn
