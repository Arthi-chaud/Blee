module Matcher.API.Event where

import Data.Aeson
import Data.Text
import GHC.Generics

data APIEvent = APIEvent
    { -- | The type of the target resource
      table :: String,
      -- | The type of event
      actionType :: ActionType,
      -- | The UUID of the target resource
      id :: String,
      -- | The name of the resource
      name :: String
    }
    deriving (Generic, Show)

instance FromJSON APIEvent where
    parseJSON = withObject "Event" $ \v ->
        APIEvent
            <$> v .: "table"
            <*> v .: "action_type"
            <*> v .: "id"
            <*> v .: "name"

data ActionType = Insert | Update | Delete deriving (Generic, Show)

instance FromJSON ActionType where
    parseJSON = withText "action_type" $ \v -> case unpack v of
        "INSERT" -> return Insert
        "UPDATE" -> return Update
        "DELETE" -> return Delete
        _ -> fail "Invalid Action Type"