{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.ByteString.Char8 as C
import Data.ByteString as W
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics
import qualified Data.CaseInsensitive as CI
import Network.Wai
import Network.HTTP
import System.Environment
import Network.Wai.Handler.Warp
import Servant
import Servant.JS
import Data.IORef
import Network.OpenID
import Network.URI
import Debug.Trace
import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser


type QueueAPI = "game" :> Capture "id" Int :>
                (Get '[JSON] Queue
                 :<|> "teamA" :> ReqBody '[JSON] User :> Post '[JSON] Queue
                 :<|> "teamB" :> ReqBody '[JSON] User :> Post '[JSON] Queue
                 :<|> ReqBody '[JSON] User :> Post '[JSON] Queue -- add to undecided
                 :<|> ReqBody '[JSON] User :> Delete '[JSON] Queue
                 :<|> "voteMap" :> ReqBody '[JSON] (User, MapName) :> Post '[JSON] Queue
                 :<|> "voteShuffle" :> ReqBody '[JSON] User :> Post '[JSON] Queue) -- remove from game
                :<|> "game" :> "new" :> ReqBody '[JSON] User :> Post '[JSON] Queue
                :<|> "auth" :> "openid" :> Post '[JSON] ()
                :<|> "auth" :> "openid" :> "return" :> QueryParam "openid.identity" String :> Get '[JSON] User
-- /game/1
type MapName = String

data Team = Team {
  players :: [User],
  name :: String
                 }

addToTeam :: Team -> User -> Team
addToTeam (Team ps n) u = Team (u:ps) n

data Queue = Queue
  {
   teamA :: [User],
   teamB :: [User],
   undecided :: [User],
   mapVotes :: M.Map MapName [User],
   shuffleVotes :: [User]
  } deriving Generic
instance ToJSON Queue

emptyQueue = Queue [] [] [] M.empty []


addTeamA :: User -> Queue -> Queue
addTeamA u q@(Queue{teamA = x, teamB = y}) = q{teamA = (u:x), teamB= delete u teamB}

addTeamB :: User -> Queue -> Queue
addTeamB u q@(Queue{teamB = x}) = q{teamB = (u:x), teamA = delete u teamA}

addUndecided :: User -> Queue ->Queue
addUndecided u q@(Queue{undecided = x})  = q{undecided = (u:x)}

addVote :: (MapName, User) -> Queue -> Queue
addVote (m, u) q@(Queue{mapVotes = x})  =
  let v = M.alter (fmap (u:)) m x in
    q{mapVotes = x}

addShuffleVote :: User -> Queue -> Queue
addShuffleVote u q@(Queue{shuffleVotes = x}) = q{shuffleVotes = (u:x)}

type SState = ExceptT ServerError (ReaderT Store IO)

type Store = IORef (M.Map Int Queue)
-- to be redis, soon

lkup :: Int -> SState Queue
lkup ix = do
  st <- liftIO . readIORef =<< ask
  case M.lookup ix st of
    Just q -> pure q
    Nothing -> throwError err404

sstatetoh :: Store -> SState a -> Handler a
sstatetoh store s = do
  x <- liftIO $ runReaderT (runExceptT s) store
  case x of
    Right k -> return $ k
    Left err -> throwError err

data User = User {
  steamId :: Int -- need a steamid type prolly but who cares
                 } deriving (Generic, Show, Eq)
instance ToJSON User
instance FromJSON User

removeUser = undefined

queueAPI :: Proxy QueueAPI
queueAPI = Proxy



server :: ServerT QueueAPI SState
server = (\ix ->
            lkup ix
           :<|> (\u -> addTeamA u <$> lkup ix)
           :<|> (\u -> addTeamB u <$> lkup ix)
           :<|> (\u -> addUndecided u <$> lkup ix)
           :<|> (\u -> removeUser u <$> lkup ix)
           :<|> (\(u, m) -> addVote (m, u) <$> lkup ix)
           :<|> (\u -> addShuffleVote u <$> lkup ix))
         :<|> create
         :<|> resolve
         :<|> (\s -> pure $ User (read (Data.List.last $ pathSegments (fromJust $ parseURI (fromJust s)))))
         where
           create :: User -> SState Queue
           create user = let newQ = addTeamA user emptyQueue in
             do
              st <- ask
              liftIO $ modifyIORef st (M.insert 0 newQ) >> pure newQ

server' :: Store -> Server QueueAPI
server' store = hoistServer queueAPI (sstatetoh store) server

app :: Store -> Application
app store = serve queueAPI (server' store)

start :: IO ()
start = newIORef M.empty >>= \x -> (run 8081 (app x))

apiJS = writeJSForAPI queueAPI vanillaJS  "vanillaAPI.js"
apiAxios = writeJSForAPI queueAPI  (axios defAxiosOptions) "axiosAPI.js"

steamURI = fromJust $ (parseProvider "https://steamcommunity.com/openid/login")

resolve :: SState ()
resolve = do
  let uri = (uriToString id (authenticationURI emptyAssociationMap Setup steamURI (Identifier "http://specs.openid.net/auth/2.0/identifier_select") "http://localhost:8081/auth/openid/return"
                             (Just [("openid.ns.ax", "http://openid.net/srv/ax/1.0"),("openid.ax.mode", "fetch_request")]) (Just "http://localhost"))) ""
  throwError $ err301 {errHeaders = pure (CI.mk (C.pack "Location"), C.pack uri)}
