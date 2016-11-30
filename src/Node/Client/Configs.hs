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

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.Client.Configs (  module Node.Client.Types
                            , buildConfigName
                            , readConfigFile
                            , readNodeManagerConf
                           ) where
-- General
import           Data.Aeson
import           Data.Text
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
-- Local Types
import           Node.Client.Types               (MyHostPreference (..),
                                                  NodeManagerConfig (..))

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
     where (Host host) = getHostPreference . nodeManagerHost $ nmcfg
           port = show . nodeManagerPort $ nmcfg

sendHttpRequest :: String -> Value -> IO (Either Text LBS.ByteString)
sendHttpRequest url sendopts = do
    r <- W.post url sendopts
    case r ^? W.responseStatus of
      (Just (Status 200 _)) -> return $ Right $ r ^. W.responseBody
      _ -> return $ Left  ("Error: Cannot received configs from NodeManager."::Text)

readFromNodeManager :: NodeManagerConfig -> Value -> IO (Either Text LBS.ByteString)
readFromNodeManager nmcfg sendObject = sendHttpRequest sendUrl sendObject
    where nmAddress = buildNodeMangerAddress nmcfg
          sendUrl = "http://" ++ nmAddress ++ "/configure/edit"

filePathToString :: OS.FilePath -> String
filePathToString fpath = either (\_-> "default.yml"::String) unpack $ OS.toText fpath

decodeConfig :: (FromJSON a, ToJSON a) => OS.FilePath -> LBS.ByteString-> IO a
decodeConfig fpath value = either fail (\alcfg -> do
                                    encodeFile (filePathToString fpath) alcfg
                                    return alcfg) $ A.eitherDecode value

readConfigFile :: (FromJSON a, ToJSON a) => NodeManagerConfig -> OS.FilePath -> Value -> IO a
readConfigFile nmcfg fpath sendObj =
  case OS.toText fpath of
         Left  e -> fail (unpack e)
         Right cfName -> do
            rslt <- try $ readFromNodeManager nmcfg sendObj
            case rslt of
             Left (_e::HttpException) -> readFromLocal fpath
             Right alcfg -> do
                   putStrLn $ "Successfully Decode " ++ unpack cfName
                   either (\_ -> readFromLocal fpath) (decodeConfig fpath) alcfg

buildConfigName :: Text -> String
buildConfigName fPath = unpack . fst $ breakOn ("."::Text) fPath
