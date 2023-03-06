-- |
-- Module      : Main
-- Description : Application start
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Just calls the bootstrap function of the Janus application.
--
module Main (main) where

import Janus (startup)

main :: IO ()
main = startup
