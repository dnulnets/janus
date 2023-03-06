{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus.Static
-- Description : The static part of the Janus application.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the static part of the application that servers static pages.
--
module Janus.Static (app) where

import Janus.Core (JScottyM)
import Web.Scotty.Trans (addHeader, file, get)
import Control.Monad.IO.Class (MonadIO)

-- | The part of the application that serve static files.
app :: (MonadIO m) => 
  -- | The application
  JScottyM m ()
app = do
  -- The icon
  get "/static/favicon.ico" $ do
    addHeader "Content-Type" "image/x-icon"
    file "./www/static/favicon.ico"

  -- Some logo files
  get "/static/logo.png" $ do
    addHeader "Content-Type" "image/ png"
    file "./www/static/logo.png"
  get "/static/logo.svg" $ do
    addHeader "Content-Type" "image/svg+xml"
    file "./www/static/logo.svg"

  -- The stylesheets
  get "/css/bootstrap.min.css" $ do
    addHeader "Content-Type" "text/css"
    file "./www/css/bootstrap.min.css"
  get "/css/bootstrap.min.css.map" $ do
    addHeader "Content-Type" "text/css"
    file "./www/css/bootstrap.min.css.map"
  get "/css/base.css" $ do
    addHeader "Content-Type" "text/css"
    file "./www/css/base.css"

  -- The javascript libraries
  get "/js/bootstrap.bundle.min.js" $ do
    addHeader "Content-Type" "text/javascript"
    file "./www/js/bootstrap.bundle.min.js"
  get "/js/bootstrap.bundle.min.js.map" $ do
    addHeader "Content-Type" "text/javascript"
    file "./www/js/bootstrap.bundle.min.js.map"

  -- Application startup files
  get "/index.js" $ do
    addHeader "Content-Type" "text/javascript"
    file "./www/index.js"
  get "/" $ do
    addHeader "Content-Type" "text/html"
    file "./www/index.html"
