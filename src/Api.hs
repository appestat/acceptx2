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
import GHC.Conc
import Debug.Trace
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
import Servant.API.WebSocket
import Data.IORef
import Control.Monad.Reader
import Network.OpenID
import Network.URI
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Network.Wai.Middleware.RequestLogger
import Data.Aeson
import Control.Concurrent.STM.TChan
import Network.WebSockets

import Queue
import User
import Monad
import Match
import SteamCommunity




type WebSocketAPI = "game" :> Capture "id" Int :> "sub" :> WebSocket

-- Try serializing Id immediately?

instance FromJSON (SState SteamUser) where
  parseJSON (Object o) = do
    id <- o .: "Id"
    pure $ liftIO $ idsToUsers id



type QueueAPI = ("game" :> Capture "id" Int :>
                (Get '[JSON] Queue
                 :<|> "teamA" :> ReqBody '[JSON] (Int, Id) :> Post '[JSON] Queue
                 :<|> "teamB" :> ReqBody '[JSON] (Int, Id) :> Post '[JSON] Queue
                 :<|> "undecided" :> ReqBody '[JSON] Id :> Post '[JSON] Queue -- add to undecided
                 :<|> ReqBody '[JSON] Id :> Delete '[JSON] Queue
                 :<|> "voteMap" :> ReqBody '[JSON] (Id, MapName) :> Post '[JSON] Queue
                 :<|> "voteShuffle" :> ReqBody '[JSON] Id :> Post '[JSON] Queue
                 :<|> "config" :> Get '[JSON] Match
                 :<|> "ready" :> ReqBody '[JSON] Id :> Post '[JSON] ()))
                :<|> "game" :> "new" :> ReqBody '[JSON] Id :> Post '[JSON] Queue
                :<|> "auth" :> "openid" :> Get '[JSON] ()
                :<|> "auth" :> "openid" :> "return" :> QueryParam "openid.identity" String :> Get '[JSON] Int

type QueueAPISocket = QueueAPI :<|> WebSocketAPI



instance ToCapture (Capture "id" Int) where
  toCapture _ = DocCapture "id" "Id of the desired game"

-- /game/1



type SteamID = Int

instance ToSample Int where
  toSamples _ = singleSample $ 0

instance ToSample SteamUser where
  toSamples _ = singleSample $ SteamUser "kai" undefined 0 True

instance ToSample Char where
  toSamples _ = singleSample 'a'

instance ToSample () where
  toSamples _ = singleSample ()

instance ToParam (QueryParam' mods "openid.identity" String) where
  toParam _ = DocQueryParam "openid.identity" ["100", "295"] "The user's steamid" Normal


queueToMatch :: Queue -> Match
queueToMatch(Queue{teamA=a, teamB=b}) = Match (steamid <$> (users a)) (steamid <$> (users b))


removeUser = undefined

webs :: Proxy (WebSocketAPI)
webs = Proxy

sock :: ServerT WebSocketAPI SState
sock = \i s -> do
  ups <- getUpdates
  chan <- liftIO $ atomically $ cloneTChan ups
  forever $ do
    update <- liftIO $ atomically $ readTChan chan
    liftIO $ sendTextData s (encode update)

  




queueAPI :: Proxy QueueAPI
queueAPI = Proxy

queueAPISocket :: Proxy (QueueAPISocket)
queueAPISocket = Proxy

server :: ServerT (QueueAPISocket) SState
server = ((\ix -> lkup ix
           :<|> (\(i, u) -> do
                    u' <- liftIO $ idsToUsers u
                    matchModify ix (addTeamA i u')) -- fixme
           :<|> (\(i, u) -> do
                    u' <- liftIO $ idsToUsers u
                    matchModify ix (addTeamB i u'))
           :<|> (\u -> do
                    u' <- liftIO $ idsToUsers u
                    matchModify ix (addUndecided u'))
           :<|> (\u -> undefined ) -- FIXME
           :<|> (\(u, m) -> do
                    u' <- liftIO $ idsToUsers u
                    matchModify ix (addVote (m, u')))
           :<|> (\u -> do
                    u' <- liftIO $ idsToUsers u
                    matchModify ix (addShuffleVote u'))
           :<|> (queueToMatch <$> lkup ix)
           :<|> (\u -> do
                    q <- matchModify ix (addReady u)
                    when (numRead q == 10) $ tellExecute ix))
         :<|> create
         :<|> (resolve "http://localhost")
         :<|> (\s -> throwError $ err302 {errHeaders = pure $ ((CI.mk (C.pack "Location")), (C.pack $ traceId $ "http://localhost:3000/?id="++ (Data.List.last $ pathSegments (fromJust $ parseURI (fromJust s)))))}))
         :<|> sock









server' :: Store -> Server QueueAPISocket
server' store = hoistServer queueAPISocket (sstatetoh store) server

app :: Store ->  Application
app store = logStdoutDev $ cors (const $ Just policy) $ provideOptions queueAPI $ serve queueAPISocket (server' store)

policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ] }

start :: IO ()
start = do
  storage <- newIORef (M.fromList [(0, emptyQueue)])
  channel <- liftIO $ atomically $ newBroadcastTChan
  (run 8081 (app (Store storage channel)))

--apiAxios :: IO ()
-- apiAxios = writeJSForAPI queueAPI (axiosWith defAxiosOptions defCommonGeneratorOptions{urlPrefix = "http://localhost:8081"}) "axiosAPI.js"

-- apiDocs :: IO ()
-- apiDocs = writeFile "docs.md" $ (markdown . docs $ queueAPI)

steamURI = fromJust $ (parseProvider "https://steamcommunity.com/openid/login")

resolve :: String -> SState ()
resolve s = do
  let uri = (uriToString id (authenticationURI emptyAssociationMap Setup steamURI (Identifier "http://specs.openid.net/auth/2.0/identifier_select") "http://localhost:8081/auth/openid/return"
                             (Just [("openid.ns.ax", "http://openid.net/srv/ax/1.0"),("openid.ax.mode", "fetch_request")]) (Just s))) ""
  throwError $ err302 {errHeaders = pure (CI.mk (C.pack "Location"), C.pack uri)}
