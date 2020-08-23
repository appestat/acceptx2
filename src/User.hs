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
data SteamUser =
  SteamUser {
  personaName :: String
  , avatarFull :: URI
  , steamid :: Int
  } deriving (Eq, Show, Generic)


instance FromJSON SteamUser where
  parseJSON (Object o) = SteamUser <$> o .: "personaname"
                                   <*> (fromJust . parseURI <$> o .: "avatarfull")
                                   <*> (read <$> (o .: "steamid"))

instance ToJSON SteamUser where
  toJSON (SteamUser s u slot') = object ["username" .= toJSON s, "avatar" .= toJSON (uriToString id u $ ""), "steamId".= toJSON slot']

instance Ord SteamUser where
  compare = compare `on` steamid

instance ToJSONKey SteamUser

