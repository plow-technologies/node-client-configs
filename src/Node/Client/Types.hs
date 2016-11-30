{- |
Module      :  Node.Client.Types
Description :  Types to share among different package
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  lingpo.huang@plowtech.net
Stability   :  unstable
Portability :  portable

<Types to use for NodeManager Config>
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Node.Client.Types  ( NodeManagerConfig (..)
                          , MyHostPreference (..)
                           ) where
-- General
import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad                   (mzero)
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  Value (Object, String),
                                                  object, parseJSON, toJSON,
                                                  (.:), (.=))
import           Data.Text                       (Text, unpack)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
-- IP Address
import           Data.Streaming.Network.Internal (HostPreference (..))

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
   toJSON (MyHostPreference (Host hp)) = toJSON hp
   toJSON _ = toJSON (""::String)

