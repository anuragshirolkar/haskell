{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module UserService (
  runUserService
) where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           UserServiceData

-- * api

type UserService =
    "listAllUsers" :> ReqBody '[JSON] ListAllUsersRequest :>  Post '[JSON] ListAllUsersResponse :<|>
    "getUsersById" :> ReqBody '[JSON] GetUsersByIdRequest :> Post '[JSON] GetUsersByIdResponse :<|>
    "test" :> Get '[JSON] String

userService :: Proxy UserService
userService = Proxy

-- * app

runUserService :: IO ()
runUserService = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  mkApp >>= (runSettings settings)

mkApp :: IO Application
mkApp = return $ serve userService server

server :: Server UserService
server =
  listAllUsers :<|>
  getUsersById :<|>
  test

allUsers :: [User]
allUsers = [
    User 0 "Anurag",
    User 1 "Afif",
    User 2 "Amit"
    ]

listAllUsers :: ListAllUsersRequest -> Handler ListAllUsersResponse
listAllUsers req = return $ ListAllUsersResponse allUsers

getUsersById :: GetUsersByIdRequest -> Handler GetUsersByIdResponse
getUsersById req = return $ GetUsersByIdResponse []

test :: Handler String
test = return "Test"