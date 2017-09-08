{-# LANGUAGE OverloadedStrings #-}

module EventStoreSettings where

import Database.EventStore

settings :: Settings
settings = defaultSettings { s_credentials = Just (credentials "admin" "changeit") }

connectionType :: ConnectionType
connectionType = Static "127.0.0.1" 1113
