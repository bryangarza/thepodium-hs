{-# LANGUAGE OverloadedStrings #-}
module Podium.DB.Test where

import Podium.DB.Manipulation
import Podium.DB.Util
import Podium.DB.Table.Account
import Podium.DB.Table.Post

import Data.Default
import Data.Int (Int64)
import Data.Text
import Data.UUID as U

testCreatePost :: Database Int64
testCreatePost = createPost (def :: Post) U.nil

testUpdatePostBody :: PostId -> Database Int64
testUpdatePostBody = updatePostBody "updated"

testUpdateAccountUsername :: Database Int64
testUpdateAccountUsername = updateAccountUsername "updatedUsername" U.nil

testUpdateAccountEmail :: Database Int64
testUpdateAccountEmail = updateAccountEmail "updatedEmail" U.nil

testUpdateAccountPassword :: Database Int64
testUpdateAccountPassword = updateAccountPassword "updatedPassword" U.nil

testCreateAccount :: Database Int64
testCreateAccount = createAccount (def :: Account)

testWithAccount :: (UUID -> Database Int64) -> String -> Database Int64
testWithAccount action accountIdStr =
  case U.fromString accountIdStr of
    Just accountId -> action accountId
    Nothing        -> action U.nil

testDeleteAccount :: Database Int64
testDeleteAccount = deleteAccount U.nil
