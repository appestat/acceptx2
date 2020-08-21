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
import Data.Function (on)
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
import Servant.Docs
import Data.Bimap as B
import Data.IORef
import Network.OpenID
import Network.URI
import Debug.Trace
import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser


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
newtype MapName = MapName {getName :: String}
  deriving (Generic, Show, Eq)
instance Ord MapName where
  compare = compare `on` getName

instance ToJSON MapName
instance FromJSON MapName
instance ToJSONKey MapName

type SteamID = Int

type Team = B.Bimap Int User



rev :: (a, b) -> (b, a)
rev (x, y) = (y, x)

addToTeam :: Team -> Int -> User -> Team
addToTeam ps i u = B.insert i u ps

data Queue = Queue
  {
   teamA :: Team,
   teamB :: Team,
   undecided :: [User],
   mapVotes :: M.Map MapName [User],
   shuffleVotes :: [User]
  } deriving Generic
instance ToJSON Queue

sampleA = B.fromList [(1,User 0), (2, User 1), (3, User 5), (4, User 100)]
sampleB = B.fromList [(1,User 2), (2, User 3), (3, User 1500), (4, User 150), (5, User 10)]

sampleVote = M.fromList [(MapName "de_dust2", [User 1,User 2,User 3,User 4,User 5]), (MapName "mirage", [User 100])]

instance ToSample Queue where
  toSamples _ = singleSample $ Queue sampleA sampleB [User 43] sampleVote [User 0, User 1]

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

instance ToSample MapName where
  toSamples _ = samples [MapName "de_dust2", MapName "de_mirage", MapName "cs_office"]

emptyQueue = Queue B.empty B.empty [] M.empty []



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
                 } deriving (Generic, Show, Eq, Ord)



instance ToJSON User
instance FromJSON User

instance ToJSONKey User
 
instance ToJSON (B.Bimap Int User) where
  toJSON = toJSON . B.toMap



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
         where
           create :: User -> SState Queue
           create user = let newQ = addTeamA 1 user emptyQueue in
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

apiDocs = docs queueAPI

putDocs = Prelude.Compat.putStrLn $ markdown apiDocs

steamURI = fromJust $ (parseProvider "https://steamcommunity.com/openid/login")

resolve :: SState ()
resolve = do
  let uri = (uriToString id (authenticationURI emptyAssociationMap Setup steamURI (Identifier "http://specs.openid.net/auth/2.0/identifier_select") "http://localhost:8081/auth/openid/return"
                             (Just [("openid.ns.ax", "http://openid.net/srv/ax/1.0"),("openid.ax.mode", "fetch_request")]) (Just "http://localhost"))) ""
  throwError $ err301 {errHeaders = pure (CI.mk (C.pack "Location"), C.pack uri)}
