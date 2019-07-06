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

-- @NOTE: this boilerplate is from the `servant` tutorial:
-- https://github.com/haskell-servant/servant/blob/master/doc/tutorial/Server.lhs

import Lib

-- servant
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

-- type UserAPI1 = "users" :> Get '[JSON] [User]

-- data User = User
--   { name :: String
--   , age :: Int
--   , email :: String
--   , registration_date :: Day
--   } deriving (Eq, Show, Generic)
-- instance ToJSON User

-- users1 :: [User]
-- users1 =
--   [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
--   , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
--   ]

-- server1 :: Server UserAPI1
-- server1 = return users1

-- userAPI :: Proxy UserAPI1
-- userAPI = Proxy
-- -- 'serve' comes from servant and hands you a WAI Application,
-- -- which you can think of as an "abstract" web application,
-- -- not yet a webserver.
-- app1 :: Application
-- app1 = serve userAPI server1

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]

main = withSQLite "people.sqlite" $ do
  createTable people
  insert_ people
    [ Person "Velvet"    19 (Just Dog)
    , Person "Kobayashi" 23 (Just Dragon)
    , Person "Miyu"      10 Nothing
    ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person Selda.! #age .>= 18)
    return (person Selda.! #name Selda.:*: person Selda.! #pet)
  liftIO $ print adultsAndTheirPets

-- main :: IO ()
-- main = run 8081 app1
