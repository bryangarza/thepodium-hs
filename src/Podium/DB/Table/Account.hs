{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Podium.DB.Table.Account where

import GHC.Generics
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Data.Time (UTCTime)
import Data.UUID as U
import Data.UUID.Aeson
import Opaleye
  ( Column
  , optional
  , PGArray
  , PGInt4
  , PGText
  , PGTimestamptz
  , PGUuid
  , required
  , Table(Table)
  )
import Servant.Common.Text (FromText, fromText)

instance FromText UUID where
  fromText = U.fromText

type AccountId  = UUID
type Username   = Text
type Email      = Text
type Password   = Text
type Bio        = Text
type JoinDate   = UTCTime
type Location   = Text
type Profession = Text
type YearsOfExp = Int
type Interests  = [Text]

data Account' a b c d e f g h i j = Account

    { _accountId  :: a
    , _username   :: b
    , _email      :: c
    , _password   :: d
    , _bio        :: e
    , _joinDate   :: f
    , _location   :: g
    , _profession :: h
    , _yearsOfExp :: i
    , _interests  :: j
    } deriving (Eq, Show, Generic)

makeLenses ''Account'

type Account = Account'
  AccountId
  Username
  Email
  Password
  Bio
  JoinDate
  Location
  Profession
  YearsOfExp
  Interests

instance ToJSON Account

instance Default Account where
  def =
    Account U.nil
    "foo"
    "foo@bar.com"
    "quux123456"
    "my bio"
    (read ("2016-03-18 20:21:31.390763 UTC") :: UTCTime)
    "SF"
    "Developer"
    3
    ["hiking", "sketching", "reading"]

$(makeAdaptorAndInstance "pAccount" ''Account')

-- type signature format:
-- someTable :: Table <writes>
--                    <reads>

type ColumnW = Account'
    (Column PGUuid)
    (Column PGText)
    (Column PGText)
    (Column PGText)
    (Column PGText)
    (Column PGTimestamptz)
    (Column PGText)
    (Column PGText)
    (Column PGInt4)
    (Column (PGArray PGText))
type ColumnR = Account'
    (Column PGUuid)
    (Column PGText)
    (Column PGText)
    (Column PGText)
    (Column PGText)
    (Column PGTimestamptz)
    (Column PGText)
    (Column PGText)
    (Column PGInt4)
    (Column (PGArray PGText))

table :: Table ColumnW ColumnR
table = Table "accountTable" (
  pAccount Account { _accountId  = required "accountId"
                   , _username   = required "username"
                   , _email      = required "email"
                   , _password   = required "password"
                   , _bio        = required "bio"
                   , _joinDate   = required "joinDate"
                   , _location   = required "location"
                   , _profession = required "profession"
                   , _yearsOfExp = required "yearsOfExp"
                   , _interests  = required "interests"  })
