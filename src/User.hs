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

data User = User {
  steamId :: Int -- need a steamid type prolly but who cares
                 } deriving (Generic, Show, Eq, Ord)



instance ToJSON User
instance FromJSON User

instance ToJSONKey User
