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
import qualified Data.Map as M
import qualified Data.Bimap as B
import Servant.Docs
import GHC.Generics

import User

type Team = B.Bimap Int User

newtype MapName = MapName {getName :: String}
  deriving (Generic, Show, Eq)

data Queue = Queue
  {
   teamA :: Team,
   teamB :: Team,
   undecided :: [User],
   mapVotes :: M.Map MapName [User],
   shuffleVotes :: [User]
  } deriving Generic

instance ToJSON Queue

instance ToJSON MapName

instance FromJSON MapName

instance ToJSONKey MapName

instance ToJSON (B.Bimap Int User) where
  toJSON = toJSON . B.toMap

instance ToSample Queue where
  toSamples _ = singleSample $ Queue sampleA sampleB [User 43] sampleVote [User 0, User 1]

instance ToSample MapName where
  toSamples _ = samples [MapName "de_dust2", MapName "de_mirage", MapName "cs_office"]

instance Ord MapName where
  compare = compare `on` getName

emptyQueue = Queue B.empty B.empty [] M.empty []

addToTeam :: Team -> Int -> User -> Team
addToTeam ps i u = B.insert i u ps

addTeamA :: Int -> User -> Queue -> Queue
addTeamA i u q@(Queue{teamA = x, teamB = teamb}) = q{teamA = addToTeam x i u, teamB= B.deleteR u teamb}

addTeamB :: Int -> User -> Queue -> Queue
addTeamB i u q@(Queue{teamB = x, teamA = teama}) = q{teamB = addToTeam x i u, teamA = (B.deleteR u teama)}

addUndecided :: User -> Queue ->Queue
addUndecided u q@(Queue{undecided = x})  = q{undecided = (u:x)}

addVote :: (MapName, User) -> Queue -> Queue
addVote (m, u) q@(Queue{mapVotes = x})  =
  let v = M.alter (fmap (u:)) m x in
    q{mapVotes = x}

addShuffleVote :: User -> Queue -> Queue
addShuffleVote u q@(Queue{shuffleVotes = x}) = q{shuffleVotes = (u:x)}
sampleA = B.fromList [(1,User 0), (2, User 1), (3, User 5), (4, User 100)]
sampleB = B.fromList [(1,User 2), (2, User 3), (3, User 1500), (4, User 150), (5, User 10)]

sampleVote = M.fromList [(MapName "de_dust2", [User 1,User 2,User 3,User 4,User 5]), (MapName "mirage", [User 100])]

