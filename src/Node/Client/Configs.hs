{- |
Module      :  <Node.Client.Configs>
Description :  <Function to read config files from Node Manager>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable

<Function to read a config file from Node manager, if fail try to read from a local copy>
-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.Client.Configs (  NodeManagerConfig
                            , managerFilePath
                            , nodeManagerPort
                            , nodeManagerHost
                            , MyHostPreference
                            , getHostPreference
                            , buildConfigName
                            , readConfigFile
                            , readNodeManagerConf
                           ) where
-- General
import           Data.Aeson
import           Data.Text
import           GHC.Generics                    (Generic)
import           Node.Client
import           Prelude
-- File Reading
import qualified Data.ByteString                 as BS
import qualified Filesystem.Path.CurrentOS       as OS
-- IP Address
import           Data.Streaming.Network.Internal
-- Yaml
import           Data.Yaml
-- Wreq
import           Control.Monad.Trans.Either
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Lazy.Char8      as LBSC
import           Servant.Client

-- Warp hostPreference inside our own type
newtype MyHostPreference = MyHostPreference {
    getHostPreference :: HostPreference} deriving (Eq,Read,Show,Generic)

instance FromJSON MyHostPreference where
  parseJSON (String str) = return . MyHostPreference . Host . unpack $ str
  parseJSON _ = fail "Parsing MyHostPreference Expected String, recieved Other"

instance ToJSON MyHostPreference where
 toJSON (MyHostPreference hPreference) =
        case hPreference of
             (Host hp) -> toJSON hp
             _ -> toJSON (""::String)

readNodeManagerConf :: OS.FilePath -> IO NodeManagerConfig
readNodeManagerConf fPath = do
	fCont <- BS.readFile (OS.encodeString fPath)
	either fail return $ decodeEither fCont

readFromLocal :: FromJSON a => OS.FilePath -> IO a
readFromLocal fpath = do
    fCont <- BS.readFile (OS.encodeString fpath)
    either fail return $ decodeEither fCont

readFromNodeManager :: NodeManagerConfig -> Value -> IO (EitherT ServantError IO Value)
readFromNodeManager nmcfg sendObject = do
  nodeAPI <- makeNodeAPI nmcfg
  return $ retrieveCfg nodeAPI sendObject

filePathToString :: OS.FilePath -> String
filePathToString fpath =
  case OS.toText fpath of
       Left _-> "default.yml"::String
       Right f -> unpack f

decodeConfig :: (FromJSON a, ToJSON a) => OS.FilePath -> String -> IO a
decodeConfig fpath value = either fail (\alcfg -> do
                                    encodeFile (filePathToString fpath) alcfg
                                    return alcfg) $ A.eitherDecode $ LBSC.pack value

buildConfigName :: Text -> String
buildConfigName fPath = unpack . fst $ breakOn ("."::Text) fPath

readConfigFile :: NodeManagerConfig -> OS.FilePath -> Value -> IO Value
readConfigFile nmcfg fpath sendObj =
  case OS.toText fpath of
         Left  e -> fail (unpack e)
         Right _cfName -> do
            result <- readFromNodeManager nmcfg sendObj
            rslt <- runEitherT result
            case rslt of
             Left _ -> readFromLocal fpath
             Right alcfg -> return alcfg


testVal :: Value
testVal = case bob of
          Left x -> "Invalid Value"
          Right y -> y
  where
  bob = A.eitherDecode $ LBSC.pack "{\"alarm-state-config\":{\"tag\":2,\"src\":{\"almKeySrc\":{\"unSText\":\"onping.plowtech.net\"}},\"host\":\"www.stupidurl.com\", \"port\": 2}}"
