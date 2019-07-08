{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- project
import Lib
-- @NOTE: these imports and the examples (below) are from the `servant` tutorial:
-- https://github.com/haskell-servant/servant/blob/master/doc/tutorial/Server.lhs
import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Hashable
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Text
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
-- selda
import Database.Selda as Selda
import Database.Selda.SQLite as SQL

type UserAPI = "user" :> Capture "userID" Int :> Get '[JSON] User
          :<|> "user" :> ReqBody '[JSON] UserInfo :> Post '[JSON] PostResponse

-- @TODO: can we avoid using `Generic`?
newtype Username = Username String deriving Generic
instance ToJSON Username
instance FromJSON Username

data User = User
  { username :: Text
  , age :: Int
  , userID :: Int
  } deriving (Eq,Show,Generic)
instance SqlRow User
instance ToJSON User
instance FromJSON User

data UserInfo = UserInfo
  { username :: String
  , age :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON UserInfo
instance FromJSON UserInfo

newtype PostResponse = PostResponse Int
instance ToJSON PostResponse where
  toJSON (PostResponse x) = object [ "userID" .= x ]

server :: Server UserAPI
server =  getUser
     :<|> postUser
  where getUser :: Int -> Handler User
        getUser x = withSQLite "user-test.sqlite" $ do
          returnMe <- query $ do
            person <- select users
            restrict (person Selda.! #userID .== (int x))
            return person
          -- @NOTE: thinking this is wrong
          -- probably can make this better
          return $ (Data.List.head returnMe)

        postUser :: UserInfo -> Handler PostResponse
        postUser (UserInfo n a) = withSQLite "user-test.sqlite" $ do
          insert_ users [ User (pack n) a newUserId ]
          return (PostResponse newUserId)
          where newUserId = hash $ n ++ (show a)

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

users :: Table User
users = table "users" [#userID :- primary]

appSettings :: Settings
appSettings = setHost "0.0.0.0" defaultSettings

main :: IO ()
main = withSQLite "user-test.sqlite" $ do
  tryCreateTable users
  liftIO $ runSettings appSettings app
