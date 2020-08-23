{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Prelude

import Control.Monad.Except
import Data.List
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Maybe
import qualified Data.CaseInsensitive as CI
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.JS
import Servant.Docs
import Data.IORef
import Network.URI
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Network.Wai.Middleware.RequestLogger
import Queue
import User
import Monad
import Match
import SteamCommunity

type Id = Int

type QueueAPI = "game" :> Capture "id" Int :>
                (Get '[JSON] Queue
                 :<|> "teamA" :> ReqBody '[JSON] (Int, Id) :> Post '[JSON] Queue
                 :<|> "teamB" :> ReqBody '[JSON] (Int, Id) :> Post '[JSON] Queue
                 :<|> "undecided" :> ReqBody '[JSON] Id :> Post '[JSON] Queue -- add to undecided
                 :<|> ReqBody '[JSON] Id :> Delete '[JSON] Queue
                 :<|> "voteMap" :> ReqBody '[JSON] (Id, MapName) :> Post '[JSON] Queue
                 :<|> "voteShuffle" :> ReqBody '[JSON] Id :> Post '[JSON] Queue
                 :<|> "config" :> Get '[JSON] Match
                 :<|> "ready" :> ReqBody '[JSON] Id :> Post '[JSON] ()) -- ACCEPT ! ACCEPT !
                :<|> "game" :> "new" :> ReqBody '[JSON] Id :> Post '[JSON] Queue
                :<|> "auth" :> "openid" :> Post '[JSON] ()
                :<|> "auth" :> "openid" :> "return" :> QueryParam "openid.identity" String :> Get '[JSON] Int





instance ToCapture (Capture "id" Int) where
  toCapture _ = DocCapture "id" "Id of the desired game"

-- /game/1



type SteamID = Int

instance ToSample Int where
  toSamples _ = singleSample $ 0

instance ToSample SteamUser where
  toSamples _ = singleSample $ SteamUser "kai" undefined 0

instance ToSample Char where
  toSamples _ = singleSample 'a'

instance ToSample () where
  toSamples _ = singleSample ()

instance ToParam (QueryParam' mods "openid.identity" String) where
  toParam _ = DocQueryParam "openid.identity" ["100", "295"] "The user's steamid" Normal


queueToMatch :: Queue -> Match
queueToMatch(Queue{teamA=a, teamB=b}) = Match (steamid <$> (users a)) (steamid <$> (users b))


removeUser = undefined

queueAPI :: Proxy QueueAPI
queueAPI = Proxy

server :: ServerT QueueAPI SState
server = (\ix ->
            lkup ix
           :<|> (\(i, u) -> put ix =<< (lkup ix >>= addTeamA i u)) -- fixme
           :<|> (\(i, u) -> put ix =<< (lkup ix >>= addTeamB i u))
           :<|> (\u -> put ix =<< (lkup ix >>= addUndecided u))
           :<|> (\u -> put ix =<< (removeUser u <$> lkup ix))
           :<|> (\(u, m) -> put ix =<< (lkup ix >>= addVote (m, u)))
           :<|> (\u -> put ix =<< (lkup ix >>= addShuffleVote u))
           :<|> (queueToMatch <$> lkup ix)
           :<|> (\u -> do
                    q <- lkup ix
                    put ix =<< (addReady u <$> lkup ix)
                    when (numRead q == 10) $ tellExecute ix
                ))
         :<|> create
         :<|> resolve
         :<|> (\s -> pure $  (read (Data.List.last $ pathSegments (fromJust $ parseURI (fromJust s)))))





server' :: Store -> Server QueueAPI
server' store = hoistServer queueAPI (sstatetoh store) server

app :: Store -> Application
app store = logStdoutDev $ cors (const $ Just policy) $ provideOptions queueAPI $ serve queueAPI (server' store)

policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ] }

start :: IO ()
start = newIORef (M.fromList [(0, emptyQueue)]) >>= \x -> (run 8081 (app x))


-- apiDocs :: IO ()
-- apiDocs = writeFile "docs.md" $ (markdown . docs $ queueAPI)

-- steamURI = fromJust $ (parseProvider "https://steamcommunity.com/openid/login")

resolve :: SState ()
resolve = return () -- broken on windows
  -- throwError $ err301 {errHeaders = pure (CI.mk (C.pack "Location"), C.pack uril
  -- )}
