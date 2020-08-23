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

type Team = B.Bimap Int SteamUser

newtype MapName = MapName {getName :: String}
  deriving (Generic, Show, Eq)

data Queue = Queue
  {
   teamA :: Team,
   teamB :: Team,
   undecided :: [SteamUser],
   mapVotes :: M.Map MapName [SteamUser],
   shuffleVotes :: [SteamUser],
   ready :: S.Set Int
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
 
emptyQueue = Queue B.empty B.empty [] M.empty [] S.empty

addReady :: Int -> Queue -> Queue
addReady u q@Queue{ready = r} =  q{ready = S.insert u r}

numRead :: Queue -> Int
numRead Queue{ready = r} = S.size r

addToTeam :: Team -> Int -> SteamUser -> Team
addToTeam ps i u = B.insert i u ps

addTeamA :: (MonadIO m) => Int -> Int -> Queue -> m Queue
addTeamA i u q@(Queue{teamA = x, teamB = teamb}) = do
  u' <- idsToUsers u
  pure $ q{teamA = addToTeam x i u', teamB= B.deleteR u' teamb}

addTeamB :: (MonadIO m) => Int -> Int -> Queue -> m Queue
addTeamB i u q@(Queue{teamB = x, teamA = teama}) = do
  u' <- idsToUsers u
  pure $ q{teamB = addToTeam x i u', teamA = (B.deleteR u' teama)}

addUndecided :: (MonadIO m) => Int -> Queue -> m Queue
addUndecided u q@(Queue{undecided = x})  = do
  u' <- idsToUsers u
  pure $ q{undecided = (u':x)}

addVote :: (MonadIO m) => (MapName, Int) -> Queue -> m Queue
addVote (m, u) q@(Queue{mapVotes = x})  = do
  u' <- idsToUsers u
  let v = M.alter (fmap (u':)) m x in
    pure $ q{mapVotes = x}

addShuffleVote :: (MonadIO m) => Int -> Queue -> m Queue
addShuffleVote u q@(Queue{shuffleVotes = x}) = do
  u' <- idsToUsers u
  pure $ q{shuffleVotes = (u':x)}
