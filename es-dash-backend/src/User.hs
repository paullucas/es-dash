{-# LANGUAGE DeriveGeneric     #-}

module User where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data User =
  User { userID :: String
       , name :: String
       } deriving (Generic, Show)

instance ToJSON User
instance FromJSON User
