{-# LANGUAGE OverloadedStrings #-}

module Node.Client.ConfigsSpec (configSpec, serverCommSpec) where

import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Filesystem.Path.CurrentOS  as OS
import           Node.Client
import           Node.Client.Configs
import           Servant.Client
import           Test.Hspec

configSpec :: Spec
configSpec = do
  describe "buildConfigName" $ do
    it "cuts off everything after a ." $
      buildConfigName "/usr/bin/stuff.html.wonky/dfkj" `shouldBe` "/usr/bin/stuff"
    it "returns an identical string if there is no ." $
      buildConfigName "/usr/bin/stuff" `shouldBe` "/usr/bin/stuff"
  describe "readNodeManagerConf" $
    it "reads the config for Node Manager" $ do
      conf <- readNodeManagerConf "node-manager-config.yml"
      nodeManagerHost conf `shouldBe` "localhost"

serverCommSpec :: Spec
serverCommSpec = do
  describe "addCfg" $
    it "Adds a config to node manager" $ do
      added <- testAdd
      let passed = case added of
                     Left _ -> False
                     Right _ -> True
      passed `shouldBe` True
  describe "readConfigFile" $
    it "Reads from Node Manager or local file" $ do
      conf <- readNodeManagerConf "node-manager-config.yml"
      val <- readConfigFile conf "node-manager-config.yml" testEditVal
      let passed = case val of
                     Object _ -> True
                     _ -> False
      passed `shouldBe` True
  describe "deleteCfg" $
    it "deletes a config from Node Manager" $ do
    val <- testDelete
    let passed = case val of
                   Left _ -> False
                   Right _ -> True
    passed `shouldBe` True

testAddVal :: Value
testAddVal = case bob of
             Left x -> "Invalid Value"
             Right y -> y
  where
  bob = A.eitherDecode $ LBSC.pack "{\"alarm-state-config\":{\"tag\":2,\"src\":{\"almKeySrc\":{\"unSText\":\"onping.plowtech.net\"}},\"host\":\"www.stupidurl.com\", \"port\": 2}}"

testEditVal :: Value
testEditVal = case bob of
              Left x -> "Invalid Value"
              Right y -> y
  where
  bob = A.eitherDecode $ LBSC.pack "{\"configName\":\"alarm-state-config\" , \"rewrite-rules\":{\"key\":\"port\" , \"val\":2}}"

testAdd :: IO (Either ServantError Value)
testAdd = do
  conf <- readNodeManagerConf "node-manager-config.yml"
  api <- makeNodeAPI conf
  runEitherT $ addCfg api testAddVal

testDelete :: IO (Either ServantError Value)
testDelete = do
  conf <- readNodeManagerConf "node-manager-config.yml"
  api <- makeNodeAPI conf
  runEitherT $  deleteCfg api "alarm-state-config"
