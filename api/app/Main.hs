{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Persist.Postgresql as Db
import Database.Persist.Sql
import Janus (startup)
import Janus.Data.Config as C
import Janus.Data.Model
import Janus.Settings
import Janus.Utils.DB (runDB)

main :: IO ()
main = startup
