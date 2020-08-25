{-# LANGUAGE FlexibleContexts #-}
-- |

module Monad where

import Control.Monad.Except
import Control.Monad.Reader

import Data.IORef
import qualified Data.Map as M

import Servant
import Data.Tuple
import Data.Conduit
import Queue
import User
import GHC.Conc
import SteamCommunity
import Control.Concurrent.STM.TChan
import Servant.API.WebSocketConduit
import Control.Monad.Reader.Class

type SState = ExceptT ServerError (ReaderT Store IO)

data Store = Store {
  matches :: IORef (M.Map Int Queue)
  , updates :: (TChan Queue) -- FIXME one for each...
  }
-- to be redis, soon


dup :: a -> (a, a)
dup x = (x,x)


getUpdates :: SState (TChan Queue)
getUpdates = asks updates




lkup :: Int -> SState Queue
lkup ix = do
  st <- liftIO . readIORef =<< asks matches
  case M.lookup ix st of
    Just q -> pure q
    Nothing -> throwError err404

putNew :: Int -> Queue -> SState Queue
putNew i q = do
  ref <- asks matches
  liftIO $ modifyIORef ref (M.insert i q)
  pure q

matchModify :: Int -> (Queue -> Queue) -> SState Queue
matchModify i f = do
  (Store ref update) <- ask
  q <- liftIO $ atomicModifyIORef' ref (\x -> swap $ M.updateLookupWithKey (\_ y -> Just (f y)) i x) -- works?
  case q of
    Just q' -> (liftIO . atomically $ writeTChan update q') >> pure q'
    Nothing -> throwError err500


sstatetoh :: Store -> SState a -> Handler a
sstatetoh str s = do
  x <- liftIO $ runReaderT (runExceptT s) str
  case x of
    Right k -> return $ k
    Left err -> throwError err

create :: Id -> SState Queue
create user = do
  user' <- liftIO $ idsToUsers user
  let newQ = addTeamA 1 user' emptyQueue
  do
    st <- asks matches
    liftIO $ modifyIORef' st (M.insert 0 newQ) >> pure newQ
