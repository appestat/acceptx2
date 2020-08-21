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

import Queue
import User
import Monad


type QueueAPI = "game" :> Capture "id" Int :>
                (Get '[JSON] Queue
                 :<|> "teamA" :> ReqBody '[JSON] (Int, User) :> Post '[JSON] Queue
                 :<|> "teamB" :> ReqBody '[JSON] (Int, User) :> Post '[JSON] Queue
                 :<|> ReqBody '[JSON] User :> Post '[JSON] Queue -- add to undecided
                 :<|> ReqBody '[JSON] User :> Delete '[JSON] Queue
                 :<|> "voteMap" :> ReqBody '[JSON] (User, MapName) :> Post '[JSON] Queue
                 :<|> "voteShuffle" :> ReqBody '[JSON] User :> Post '[JSON] Queue) -- remove from game
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



removeUser = undefined

queueAPI :: Proxy QueueAPI
queueAPI = Proxy

server :: ServerT QueueAPI SState
server = (\ix ->
            lkup ix
           :<|> (\(i, u) -> addTeamA i u <$> lkup ix)
           :<|> (\(i, u) -> addTeamB i u <$> lkup ix)
           :<|> (\u -> addUndecided u <$> lkup ix)
           :<|> (\u -> removeUser u <$> lkup ix)
           :<|> (\(u, m) -> addVote (m, u) <$> lkup ix)
           :<|> (\u -> addShuffleVote u <$> lkup ix))
         :<|> create
         :<|> resolve
         :<|> (\s -> pure $ User (read (Data.List.last $ pathSegments (fromJust $ parseURI (fromJust s)))))

server' :: Store -> Server QueueAPI
server' store = hoistServer queueAPI (sstatetoh store) server

app :: Store -> Application
app store = serve queueAPI (server' store)

start :: IO ()
start = newIORef M.empty >>= \x -> (run 8081 (app x))

apiAxios :: IO ()
apiAxios = writeJSForAPI queueAPI  (axios defAxiosOptions) "axiosAPI.js"

apiDocs :: IO ()
apiDocs = writeFile "docs.md" $ (markdown . docs $ queueAPI)

steamURI = fromJust $ (parseProvider "https://steamcommunity.com/openid/login")

resolve :: SState ()
resolve = do
  let uri = (uriToString id (authenticationURI emptyAssociationMap Setup steamURI (Identifier "http://specs.openid.net/auth/2.0/identifier_select") "http://localhost:8081/auth/openid/return"
                             (Just [("openid.ns.ax", "http://openid.net/srv/ax/1.0"),("openid.ax.mode", "fetch_request")]) (Just "http://localhost"))) ""
  throwError $ err301 {errHeaders = pure (CI.mk (C.pack "Location"), C.pack uri)}