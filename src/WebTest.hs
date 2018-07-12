{-# LANGUAGE OverloadedStrings #-}

module WebTest where

import           Web.Scotty

import           Data.Monoid            (mconcat)

import           Hufte

import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (liftIO)

import           Data.Text.Lazy         (pack)

import           System.Random          (newStdGen, randomRs)

main = scotty 3000 $
  get "/" $ do
    points <- param "points"
    graphs <- param "graphs"

    let pn = read points :: Int
    let gn = read graphs :: Int

    sparklines <- liftIO $ randomSparklines gn pn
    html $ mconcat $ map toParagraph sparklines
      where toParagraph sparkline = mconcat ["<span>", pack sparkline, "</span>"]

randomSparkline :: Int -> IO String
randomSparkline pn = do
  gen <- newStdGen
  let randomVals = take pn $ randomRs (0, height defaultConf) gen
  liftIO $ lineGraph defaultConf randomVals

randomSparklines :: Int -> Int -> IO [String]
randomSparklines gn pn = replicateM gn $ randomSparkline pn
