{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module SteamCommunity (SteamIDs(..), idsToUsers) where
import Servant.Client
import Servant
import Data.Aeson
import Network.URI
import GHC.Generics
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.Except
import User

data Resp = Resp {
  players :: [SteamUser]
                 }

data Content = Content {
  resp :: Resp
                       }


instance FromJSON Content where
  parseJSON (Object o) = Content <$> (o .: "response")

instance FromJSON Resp where
  parseJSON (Object o) = Resp <$> (o .: "players")


newtype SteamIDs = SteamIDs {ids::Int}
instance ToHttpApiData SteamIDs where
  toQueryParam (SteamIDs xs) = (T.pack . show) xs

instance FromHttpApiData SteamIDs where
  parseQueryParam t = Right . SteamIDs $ read (T.unpack t)

type SteamAPI = "GetPlayerSummaries" :> "v0002" :> QueryParam "key" String :> QueryParam "steamids" SteamIDs :> Get '[JSON] Content


steamAPI :: Proxy SteamAPI
steamAPI = Proxy

getSteamUser' :: Maybe String -> Maybe SteamIDs -> ClientM Content
getSteamUser' = client steamAPI

getSteamUsers :: String -> Int -> IO (Content) -- TODO this is blocking?
getSteamUsers s ids = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM (getSteamUser' (Just s) (Just (SteamIDs ids))) (mkClientEnv manager' (BaseUrl Http "api.steampowered.com" 80 "ISteamUser"))
  case res of
    (Right res') -> pure $ res'
    (Left failure)-> putStrLn (show failure) >> undefined

type Id = Int

idsToUsers :: (MonadIO m) => Int -> m SteamUser
idsToUsers ids = do
  key <- liftIO $ readFile "steamkey"
  liftIO $ (head . players . resp) <$> getSteamUsers (filter (/= '\n') key) ids
