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
import Network.OpenID
import Network.URI
import Network.Wai.Middleware.Cors
import Queue
import User
import Monad
import Match


type QueueAPI = "game" :> Capture "id" Int :>
                (Get '[JSON] Queue
                 :<|> "teamA" :> ReqBody '[JSON] (Int, User) :> Post '[JSON] Queue
                 :<|> "teamB" :> ReqBody '[JSON] (Int, User) :> Post '[JSON] Queue
                 :<|> ReqBody '[JSON] User :> Post '[JSON] Queue -- add to undecided
                 :<|> ReqBody '[JSON] User :> Delete '[JSON] Queue
                 :<|> "voteMap" :> ReqBody '[JSON] (User, MapName) :> Post '[JSON] Queue
                 :<|> "voteShuffle" :> ReqBody '[JSON] User :> Post '[JSON] Queue
                 :<|> "config" :> Get '[JSON] Match
                 :<|> "ready" :> ReqBody '[JSON] User :> Post '[JSON] ()) -- ACCEPT ! ACCEPT !
                :<|> "game" :> "new" :> ReqBody '[JSON] User :> Post '[JSON] Queue
                :<|> "auth" :> "openid" :> Post '[JSON] ()
                :<|> "auth" :> "openid" :> "return" :> QueryParam "openid.identity" String :> Get '[JSON] User



instance ToCapture (Capture "id" Int) where
  toCapture _ = DocCapture "id" "Id of the desired game"

-- /game/1


type SteamID = Int

instance ToSample Int where
  toSamples _ = singleSample $ 1

instance ToSample User where
  toSamples _ = singleSample $ User 0

instance ToSample Char where
  toSamples _ = singleSample 'a'

instance ToSample () where
  toSamples _ = singleSample ()

instance ToParam (QueryParam' mods "openid.identity" String) where
  toParam _ = DocQueryParam "openid.identity" ["100", "295"] "The user's steamid" Normal


queueToMatch :: Queue -> Match
queueToMatch(Queue{teamA=a, teamB=b}) = Match (steamId <$> (users a)) (steamId <$> (users b))


removeUser = undefined

queueAPI :: Proxy QueueAPI
queueAPI = Proxy

server :: ServerT QueueAPI SState
server = (\ix ->
            lkup ix
           :<|> (\(i, u) -> put ix =<< (addTeamA i u <$> lkup ix))
           :<|> (\(i, u) -> put ix =<< (addTeamB i u <$> lkup ix))
           :<|> (\u -> put ix =<< (addUndecided u <$> lkup ix))
           :<|> (\u -> put ix =<< (removeUser u <$> lkup ix))
           :<|> (\(u, m) -> put ix =<< (addVote (m, u) <$> lkup ix))
           :<|> (\u -> put ix =<< (addShuffleVote u <$> lkup ix))
           :<|> (queueToMatch <$> lkup ix)
           :<|> (\u -> do
                    q <- lkup ix
                    put ix =<< (addReady u <$> lkup ix)
                    when (numRead q == 10) $ tellExecute ix
                ))
         :<|> create
         :<|> resolve
         :<|> (\s -> pure $ User (read (Data.List.last $ pathSegments (fromJust $ parseURI (fromJust s)))))

server' :: Store -> Server QueueAPI
server' store = hoistServer queueAPI (sstatetoh store) server

app :: Store -> Application
app store = simpleCors $ serve queueAPI (server' store)

start :: IO ()
start = newIORef M.empty >>= \x -> (run 8081 (app x))

apiAxios :: IO ()
apiAxios = writeJSForAPI queueAPI  (axiosWith defAxiosOptions defCommonGeneratorOptions{urlPrefix = "localhost:8081"}) "axiosAPI.js"

apiDocs :: IO ()
apiDocs = writeFile "docs.md" $ (markdown . docs $ queueAPI)

steamURI = fromJust $ (parseProvider "https://steamcommunity.com/openid/login")

resolve :: SState ()
resolve = do
  let uri = (uriToString id (authenticationURI emptyAssociationMap Setup steamURI (Identifier "http://specs.openid.net/auth/2.0/identifier_select") "http://localhost:8081/auth/openid/return"
                             (Just [("openid.ns.ax", "http://openid.net/srv/ax/1.0"),("openid.ax.mode", "fetch_request")]) (Just "http://localhost"))) ""
  throwError $ err301 {errHeaders = pure (CI.mk (C.pack "Location"), C.pack uri)}
