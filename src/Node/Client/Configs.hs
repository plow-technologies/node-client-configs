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

module Node.Client.Configs where

-- General
import           Control.Applicative
import           Control.Monad                   (mzero)
import           Data.Aeson
import           Data.Text
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Prelude
-- File Reading
import qualified Data.ByteString                 as BS
import qualified Filesystem.Path.CurrentOS       as OS
-- IP Address
import           Data.Streaming.Network.Internal
-- Yaml
import           Data.Yaml
-- Wreq
import           Control.Exception               (try)
import           Control.Lens                    ((^.), (^?))
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Lazy            as LBS
import           Network.HTTP.Client             (HttpException)
import           Network.HTTP.Types
import qualified Network.Wreq                    as W

-- Node ManagerConfig
-- ===============================
data NodeManagerConfig = NodeManagerConfig {
      managerFilePath :: Text
    , nodeManagerHost :: MyHostPreference
    , nodeManagerPort :: Int
    } deriving (Read, Eq, Show, Typeable,Generic)


instance FromJSON NodeManagerConfig where
         parseJSON (Object o) = NodeManagerConfig <$>
                        ((o .: "node-manager-config") >>= (.: "managerFilePath"))
                    <*> ((o .: "node-manager-config") >>= (.: "nodeManagerHost"))
                    <*> ((o .: "node-manager-config") >>= (.: "nodeManagerPort"))
         parseJSON _ = mzero

instance ToJSON NodeManagerConfig where
         toJSON v = object ["node-manager-config" .= v]

-- Warp hostPreference inside our own type
newtype MyHostPreference = MyHostPreference {
      getHostPreference :: HostPreference} deriving (Eq,Read,Show,Typeable,Generic)

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

buildNodeMangerAddress :: NodeManagerConfig -> String
buildNodeMangerAddress nmcfg = host ++ ":" ++ port
     where (Host host) = Node.Client.Configs.getHostPreference . nodeManagerHost $ nmcfg
           port = show . nodeManagerPort $ nmcfg

sendHttpRequest :: String -> Value -> IO (Either Text LBS.ByteString)
sendHttpRequest url sendopts = do
    r <- W.post url sendopts
    case r ^? W.responseStatus of
      (Just (Status 200 _)) -> return $ Right $ r ^. W.responseBody
      _ -> return $ Left  ("Error: Cannot received configs from NodeManager."::Text)

readFromNodeManager :: NodeManagerConfig -> Value -> IO (Either Text LBS.ByteString)
readFromNodeManager nmcfg sendObject = do
    let nmAddress = buildNodeMangerAddress nmcfg
        sendUrl = "http://" ++ nmAddress ++ "/configure/edit"
    sendHttpRequest sendUrl sendObject

filePathToString :: OS.FilePath -> String
filePathToString fpath =
  case OS.toText fpath of
       Left _-> "default.yml"::String
       Right f -> unpack f

decodeConfig :: (FromJSON a, ToJSON a) => OS.FilePath -> LBS.ByteString-> IO a
decodeConfig fpath value = either fail (\alcfg -> do
                                    encodeFile (filePathToString fpath) alcfg
                                    return alcfg) $ A.eitherDecode value

buildConfigName :: Text -> String
buildConfigName fPath = unpack . fst $ breakOn ("."::Text) fPath

readConfigFile :: (FromJSON a, ToJSON a) => NodeManagerConfig -> OS.FilePath -> Value -> IO a
readConfigFile nmcfg fpath sendObj =
  case OS.toText fpath of
         Left  e -> fail (unpack e)
         Right _cfName -> do
            rslt <- try $ readFromNodeManager nmcfg sendObj
            case rslt of
             Left (_e::HttpException) -> readFromLocal fpath
             Right alcfg -> do
                   putStrLn "Decdoe the Alarmlog Config"
                   either (\_ -> readFromLocal fpath) (decodeConfig fpath) alcfg
