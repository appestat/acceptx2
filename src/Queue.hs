{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Queue where
import Data.Aeson
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Bimap as B
import qualified Data.Set as S
import Servant.Docs
import GHC.Generics
import Control.Monad.IO.Class
import User
import SteamCommunity


type Slot = Int

type Team = B.Bimap Int SteamUser

newtype MapName = MapName {getName :: String}
  deriving (Generic, Show, Eq)

data Queue = Queue
  {
   teamA :: Team,
   teamB :: Team,
   undecided :: S.Set SteamUser,
   mapVotes :: M.Map MapName [SteamUser],
   shuffleVotes :: S.Set SteamUser
  } deriving Generic

users :: Team -> [SteamUser]
users = B.elems




instance ToJSON Queue

instance ToJSON MapName

instance FromJSON MapName

instance ToJSONKey MapName

instance ToJSON (B.Bimap Int SteamUser) where
  toJSON = toJSON . B.toMap

instance Ord MapName where
  compare = compare `on` getName
 
emptyQueue = Queue B.empty B.empty S.empty M.empty S.empty

addReady :: Id -> Queue -> Queue
addReady u q@Queue{teamA = x, teamB = y} =  q{teamA = B.mapR (f u) x, teamB = B.mapR (f u) y}
  where
    matchesId i u = i == steamid u
    f i x = if (matchesId i x) then setReady x else x

numRead :: Queue -> Int
numRead Queue{teamA = x, teamB = y} = B.size (countRead x) + B.size (countRead y)
  where
    countRead = B.filter (\_ x -> isReady x)

addToTeam :: Team -> Int -> SteamUser -> Team
addToTeam ps i u = B.insert i u ps

addTeamA ::  Int -> SteamUser -> Queue -> Queue
addTeamA i u q@(Queue{teamA = x, teamB = teamb}) = q{teamA = addToTeam x i u, teamB= B.deleteR u teamb}

addTeamB :: Int -> SteamUser -> Queue -> Queue
addTeamB i u q@(Queue{teamB = x, teamA = teama}) = q{teamB = addToTeam x i u, teamA = (B.deleteR u teama)}

addUndecided :: SteamUser -> Queue -> Queue
addUndecided u q@(Queue{undecided = x})  = q{undecided = S.insert u x}

addVote :: (MapName, SteamUser) -> Queue -> Queue
addVote (m, u) q@(Queue{mapVotes = x})  =
  let v = M.alter (fmap (u:)) m x in
    q{mapVotes = v}

addShuffleVote :: SteamUser -> Queue -> Queue
addShuffleVote u q@(Queue{shuffleVotes = x}) = q{shuffleVotes = S.insert u x}
