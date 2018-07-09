{-# LANGUAGE OverloadedStrings #-}

module WebTest where

import           Web.Scotty

import           Data.Monoid            (mconcat)

import           Hufte

import           Control.Monad.IO.Class (liftIO)

import           Data.Text.Lazy         (pack)

main = scotty 3000 $
  get "/" $ do
    sparkline <- liftIO $ barSparkline defaultConf []
    html $ mconcat $ (mconcat .replicate 30) ["<p>", pack sparkline, "</p>"]
