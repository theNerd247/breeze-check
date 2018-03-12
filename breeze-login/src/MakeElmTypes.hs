{-# LANGUAGE OverloadedStrings #-}

module Main where

import Elm
import Data.Breeze
import Data.Data (Proxy(..))
import Data.Text (pack, unpack)

spec :: Spec
spec = 
  moduleSpecWith ops ["BreezeTypes"] $ do
    makeElm (Proxy :: Proxy BreezeException)
    makeElm (Proxy :: Proxy Person)
    makeElm (Proxy :: Proxy Name)
    makeElm (Proxy :: Proxy CheckInStatus)
    makeElm (Proxy :: Proxy NewPersonInfo)
    makeElm (Proxy :: Proxy Address)
  where
    makeElm p = do
      renderType p
      renderDecoder p
      renderEncoder p
    ops = defaultOptions 
      { elmRecordFieldModifier = modNames
      , jsonKeyModifier = modNames
      }
    modNames = pack . removeUnderscorePrefix . unpack

main = do
  let elmDir = "../ui/src/"
  putStrLn $ "Saving elm types to: " ++ elmDir
  specsToDir [spec] elmDir
