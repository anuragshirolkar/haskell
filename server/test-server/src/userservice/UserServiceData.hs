{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module UserServiceData where

import           Data.Aeson
import           GHC.Generics

data User
  = User {
    userId :: Integer,
    userName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
  
data ListAllUsersRequest = ListAllUsersRequest deriving (Eq, Show, Generic)
data ListAllUsersResponse = ListAllUsersResponse {
    listAllUsersResponseUsers :: [User]
} deriving (Eq, Show, Generic)
instance ToJSON ListAllUsersRequest
instance ToJSON ListAllUsersResponse
instance FromJSON ListAllUsersRequest
instance FromJSON ListAllUsersResponse

data GetUsersByIdRequest = GetUsersByIdRequest {
    getUsersByIdReqestIds :: [Integer]
} deriving (Eq, Show, Generic)
data GetUsersByIdResponse = GetUsersByIdResponse {
    getUsersByIdResponseUsers :: [(Integer, User)]
} deriving (Eq, Show, Generic)
instance ToJSON GetUsersByIdRequest
instance ToJSON GetUsersByIdResponse
instance FromJSON GetUsersByIdRequest
instance FromJSON GetUsersByIdResponse

class Rpc req res where
    execute :: req -> res

data UserService = UserService {
    listAllUsers :: ListAllUsersRequest -> ListAllUsersResponse
}