{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module User where
import Data.Aeson
import GHC.Generics
import Data.Maybe
import Network.URI
import Data.Function
import Data.Int

type Id = Int64

data SteamUser =
  SteamUser {
  personaName :: String
  , avatarFull :: URI
  , steamid :: Id
  , isReady :: Bool
  } deriving (Eq, Show, Generic)

setReady :: SteamUser -> SteamUser
setReady s = s{isReady=True}

instance FromJSON SteamUser where
  parseJSON (Object o) = SteamUser <$> o .: "personaname"
                                   <*> (fromJust . parseURI <$> o .: "avatarfull")
                                   <*> (read <$> (o .: "steamid"))
                                   <*> (pure False)


instance ToJSON SteamUser where
  toJSON (SteamUser s u slot' r) = object ["username" .= toJSON s, "avatar" .= toJSON (uriToString id u $ ""), "steamId".= toJSON slot', "ready" .= toJSON r]

instance Ord SteamUser where
  compare = compare `on` steamid

instance ToJSONKey SteamUser

