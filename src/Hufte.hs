{-# LANGUAGE OverloadedStrings #-}

module Hufte (barSparkline, defaultConf) where

import           Codec.Binary.Base64 (encode)
import           Data.ByteString     (unpack)

import           Graphics.GD

data SparklineConf =
  SparklineConf { width     :: Int
                , height    :: Int
                , bgColor   :: Color
                , baseColor :: Color
                , minColor  :: Color
                , maxColor  :: Color
                , currColor :: Color
                , markMin   :: Bool
                , markMax   :: Bool
                , markCurr  :: Bool
                }

type Values = [Double]

main = do
  print "I am Hufte."
  print "I am forever."

dataURI :: Image -> IO String
dataURI image =
  fmap (encode . unpack) (savePngByteString image)

transparent = rgba 0xff 0xff 0xff 0xff
defaultGrey = rgb 0xde 0xde 0xde
defaultRed = rgb 0xff 0x0d 0x0d
defaultGreen = rgb 0x0d 0xff 0x0d

defaultConf :: SparklineConf
defaultConf = SparklineConf { width = 120
                            , height = 25
                            , bgColor = transparent
                            , baseColor = defaultGrey
                            , minColor = defaultRed
                            , maxColor = defaultGreen
                            , currColor = defaultGrey
                            , markMin = True
                            , markMax = True
                            , markCurr = True
                            }

barSparkline :: SparklineConf -> Values -> IO String
barSparkline conf vs = do
  let nPoints  = length vs
      w        = width conf
      h        = height conf
      stepSize = fromIntegral (w - nPoints) / fromIntegral nPoints
  print stepSize

  img <- newImage (w, h)
  drawFilledRectangle (0, 0) (w, h) transparent img
  drawFilledRectangle (w `div` 4,h `div` 4) (3*w `div` 4, 3*h `div` 4) defaultGrey img
  byteEncoded <- dataURI img
  return $ "<img src='data:image/png;base64," ++ byteEncoded ++ "'>"
