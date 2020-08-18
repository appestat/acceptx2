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
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.JS
import Data.IORef

import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser


type QueueAPI = "queue" :> Capture "queueid" Int :>
                (Get '[JSON] Queue
                 :<|> ReqBody '[JSON] User :> Post '[JSON] Queue
                 :<|> ReqBody '[JSON] User :> Delete '[JSON] Queue)
                :<|> "queue" :> "new" :> ReqBody '[JSON] User :> Post '[JSON] Queue

data Queue = Queue
  {
   users :: [User]
  } deriving Generic
instance ToJSON Queue

type SState = ExceptT ServerError (ReaderT Store IO)

type Store = IORef (M.Map Int Queue)
-- to be redis, soon

sstatetoh :: Store -> SState a -> Handler a
sstatetoh store s = do
  x <- liftIO $ runReaderT (runExceptT s) store
  case x of
    Right k -> return $ k
    Left err -> throwError err


queue :: Queue -> User -> Queue
queue (Queue ux) x = Queue (x:ux)

dummyQueue = Queue [User 0, User 1, User 2]

data User = User {
  steamId :: Int -- need a steamid type prolly but who cares
                 } deriving (Generic, Show, Eq)
instance ToJSON User
instance FromJSON User


queueAPI :: Proxy QueueAPI
queueAPI = Proxy

server :: ServerT QueueAPI SState
server = (\ix -> getqueue ix
         :<|> enqueue ix
         :<|> dequeue ix)
         :<|> create
         where
           getqueue :: Int -> SState Queue
           getqueue ix = do
             st <- liftIO . readIORef =<< ask
             case M.lookup ix st of
               Just q' -> pure q'
               Nothing -> throwError err404

           enqueue :: Int -> User -> SState Queue
           enqueue ix user = do
             st' <- ask
             st <- liftIO . readIORef =<< ask
             case (M.lookup ix st) of
               Just q -> let newQ = queue q user in
                 liftIO $ modifyIORef st' (M.insert ix newQ) >> pure newQ
               Nothing -> throwError err404

           dequeue :: Int -> User -> SState Queue
           dequeue ix y = pure $ dummyQueue

           create :: User -> SState Queue
           create user = let newQ = Queue [user] in
             do
              st <- ask
              liftIO $ modifyIORef st (M.insert 0 newQ) >> pure newQ

server' :: Store -> Server QueueAPI
server' store = hoistServer queueAPI (sstatetoh store) server

app :: Store -> Application
app store = serve queueAPI (server' store)

start :: IO ()
start = newIORef M.empty >>= \x -> (run 8081 (app x))

apiJS = jsForAPI queueAPI vanillaJS
