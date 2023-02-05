{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus
-- Description : The concatenated parts of the Janus application.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module concats all part of the application.
module Janus (runApp, waiapp) where

import Control.Monad.Reader ( ReaderT(runReaderT) )

import Web.Scotty.Trans as T ( scottyT, scottyAppT )

import Janus.Core (JScottyM)
import Janus.Data.Config ( readConfig, Config )
import qualified Janus.Static as JS
import qualified Janus.User as JU
import Network.Wai (Application)

app :: JScottyM ()
app = JS.app <> JU.app

waiapp :: Config -> IO Application
waiapp c = scottyAppT (`runReaderT` c) app 

-- | Run the application
runApp :: IO ()
runApp = do
  conf <- readConfig "./conf.yaml"
  jscotty conf app
  
  where

    jscotty :: Config -> JScottyM () -> IO ()
    jscotty c = scottyT 8080 (`runReaderT` c)
