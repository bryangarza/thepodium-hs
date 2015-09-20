{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module DB.Table.Account where

import Data.Aeson
import Data.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Data.UUID as U
import Data.UUID.Aeson
import GHC.Generics
import Opaleye ( Column
               , Table(Table)
               , required
               , optional
               , PGText
               , PGUuid )

type Id_      = UUID
type Username = Text
type Email    = Text
type Password = Text

data Account' a b c d = Account
    { id_      :: a
    , username :: b
    , email    :: c
    , password :: d
    } deriving (Eq, Show, Generic)

type Account = Account' Id_ Username Email Password

instance ToJSON Account

instance Default Account where
  def = Account U.nil "foo" "foo@bar.com" "quux123456"

$(makeAdaptorAndInstance "pAccount" ''Account')

-- type signature format:
-- someTable :: Table <writes>
--                    <reads>

type ColumnW = Account' (Column PGUuid) (Column PGText) (Column PGText) (Column PGText)
type ColumnR = Account' (Column PGUuid) (Column PGText) (Column PGText) (Column PGText)

table :: Table ColumnW ColumnR
table = Table "accountTable" (
  pAccount Account { id_      = required "accountId"
                   , username = required "username"
                   , email    = required "email"
                   , password = required "password" })
