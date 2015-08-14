{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Node.Client where

import           Control.Applicative
import           Control.Monad              (mzero)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Text
import           Data.Typeable
import           GHC.Generics
import           Servant
import           Servant.Client

-- NodeManagerConfig
-- ===============================
data NodeManagerConfig = NodeManagerConfig {
      managerFilePath :: Text
    , nodeManagerHost :: String
    , nodeManagerPort :: Int
    } deriving (Read, Eq, Show, Generic)


instance FromJSON NodeManagerConfig where
         parseJSON (Object o) = NodeManagerConfig <$>
                        ((o .: "node-manager-config") >>= (.: "managerFilePath"))
                    <*> ((o .: "node-manager-config") >>= (.: "nodeManagerHost"))
                    <*> ((o .: "node-manager-config") >>= (.: "nodeManagerPort"))
         parseJSON _ = mzero

instance ToJSON NodeManagerConfig where
         toJSON v = object ["node-manager-config" .= v]

------------------------------ Servant ------------------------------


type API = "configure" :> "retrieve" :> ReqBody '[JSON] Value :> Post '[JSON] Value
      :<|> "configure" :> "add" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "configure" :> "delete" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "configure" :> "copy" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "clone" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "configure" :> "get" :> Get '[JSON] Value
      :<|> "configure" :> "edit" :> ReqBody '[JSON] Value :> Post '[JSON] Value
      :<|> Raw

data NodeInterface = NodeInterface {
                     retrieveCfg :: Value -> EitherT ServantError IO Value
                   , addCfg      :: Value -> EitherT ServantError IO Value
                   , editCfg     :: Value -> EitherT ServantError IO Value
                   , deleteCfg   :: Value -> EitherT ServantError IO Value
                   , copyCfg     :: Value -> EitherT ServantError IO Value
                   , cloneDir    :: Value -> EitherT ServantError IO Value
                   , getCfg      :: EitherT ServantError IO Value
}

userAPI :: Proxy API
userAPI = Proxy

makeNodeAPI :: Monad m => NodeManagerConfig -> m NodeInterface
makeNodeAPI cfg =
  return $ NodeInterface retrieveConfig addConfig editConfig deleteConfig copyConfig cloneDirectory getConfig
  where
    retrieveConfig :: Value -> EitherT ServantError IO Value
    addConfig :: Value -> EitherT ServantError IO Value
    deleteConfig :: Value -> EitherT ServantError IO Value
    copyConfig :: Value -> EitherT ServantError IO Value
    cloneDirectory :: Value -> EitherT ServantError IO Value
    getConfig :: EitherT ServantError IO Value
    editConfig :: Value -> EitherT ServantError IO Value
    -- docs :: EitherT ServantError IO Raw
    retrieveConfig :<|> addConfig :<|> deleteConfig :<|> copyConfig :<|> cloneDirectory :<|> getConfig :<|> editConfig :<|> docs = client userAPI (BaseUrl Http (nodeManagerHost cfg) (nodeManagerPort cfg))

--(toJSON (object ["alarm-state-config" .= object  [ ( "tag" .= 2), ("src" .= (object ["almKeySrc" .= (object [ "unSText" .=  "onping.plowtech.net"])])),  ("host" .= "www.stupidurl.com"), ("port".= 2)]]))
