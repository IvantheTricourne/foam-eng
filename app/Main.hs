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
import Data.List
import Data.Maybe
import Data.String.Conversions
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

type UserAPI1 = "users" :> Get '[JSON] [User]
type UserAPI = "users" :> Get '[JSON] [User]
           :<|> "user" :> Capture "userID" Int :> Get '[JSON] User
           :<|> "user" :> ReqBody '[JSON] UserInfo :> Post '[JSON] PostResponse

-- @TODO: can we avoid using `Generic`?
newtype Username = Username String deriving Generic
instance ToJSON Username
instance FromJSON Username

data User = User
  { username :: Username
  , age :: Int
  , userID :: Int
  } deriving (Generic)
instance Eq User
instance Show User
instance ToJSON User where
  toJSON (User (Username name) age userID) =
    object [ "username" .= name
           , "age" .= age
           , "userID" .= userID
           ]
instance FromJSON User where
  parseJSON (Object v) =
    User <$>
    v .: "username" <*>
    v .: "age" <*>
    v .: "userID"
  parseJSON _ = mzero

data UserInfo = UserInfo
  { username :: String
  , age :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON UserInfo
instance FromJSON UserInfo

newtype PostResponse = PostResponse Int
instance ToJSON PostResponse where
  toJSON (PostResponse x) = object [ "userID" .= x ]

users1 :: [User]
users1 =
  [ User (Username "Isaac Newton")    372 1337
  , User (Username "Albert Einstein") 136 133734
  ]

-- @TODO: Postgres stuff goes here
server :: Server UserAPI
server = return users1
     :<|> user
     :<|> postUser
  where user :: Int -> Handler User
        user x = return (User (Username "Test") 420 x)

        -- postUser :: UserInfo -> Handler PostResponse
        postUser :: UserInfo -> Handler PostResponse
        postUser (UserInfo n a) = return (PostResponse 420)

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { name :: Text
  , personAge  :: Int
  , pet  :: Maybe Pet
  } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]

-- -- SQLITE example
-- main = withSQLite "people.sqlite" $ do
--   createTable people
--   insert_ people
--     [ Person "Velvet"    19 (Just Dog)
--     , Person "Kobayashi" 23 (Just Dragon)
--     , Person "Miyu"      10 Nothing
--     ]

--   adultsAndTheirPets <- query $ do
--     person <- select people
--     restrict (person Selda.! #personAge .>= 18)
--     return (person Selda.! #name Selda.:*: person Selda.! #pet)
--   liftIO $ print adultsAndTheirPets

main :: IO ()
main = run 8081 app
