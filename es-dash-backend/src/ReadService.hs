{-# LANGUAGE OverloadedStrings #-}

module ReadService where

import           EventStoreSettings
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan.Unagi ( InChan, OutChan, newChan, readChan, dupChan, writeChan )
import           Control.Monad (forever, when)
import           Database.EventStore
import qualified Data.Text as T
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Handler.WebSockets as WaiWS
import           Network.WebSockets as WS


getStreamID :: ResolvedEvent -> T.Text
getStreamID event = resolvedEventOriginalStreamId event


logEvt :: T.Text -> IO ()
logEvt id = print $ T.append "Event Appeared! StreamID: " id


isNonStatEvt :: T.Text -> Bool
isNonStatEvt id = not $ T.isPrefixOf "$" id


readLoop broadcast gesSub  = do
  event <- nextEvent gesSub
  let streamID = getStreamID event
  logEvt streamID
  when (isNonStatEvt streamID) $ writeChan broadcast streamID
  readLoop broadcast gesSub


sendLoop :: WebSocketsData a => WS.Connection -> OutChan a -> IO b
sendLoop connection localChan = do
  message <- readChan localChan
  sendTextData connection message
  sendLoop connection localChan


handleWS :: InChan T.Text -> PendingConnection -> IO ()
handleWS broadcast pending = do
  connection <- acceptRequest pending
  sendLoop connection =<< dupChan broadcast
  return ()


main :: IO ()
main = do
  (broadcast, _) <- newChan
  gesConn <- connect settings connectionType
  forkIO $ readLoop broadcast =<< subscribeToAll gesConn False
  run 3000 $ WaiWS.websocketsOr defaultConnectionOptions (handleWS broadcast) undefined
  shutdown gesConn
  waitTillClosed gesConn
