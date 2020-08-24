-- |

module Monad where

import Control.Monad.Except
import Control.Monad.Reader

import Data.IORef
import qualified Data.Map as M

import Servant

import Queue
import User

type SState = ExceptT ServerError (ReaderT Store IO)

type Store = IORef (M.Map Int Queue)
-- to be redis, soon




lkup :: Int -> SState Queue
lkup ix = do
  st <- liftIO . readIORef =<< ask
  case M.lookup ix st of
    Just q -> pure q
    Nothing -> throwError err404

put :: Int -> Queue -> SState Queue
put i q = do
  ref <- ask
  liftIO $ modifyIORef ref (M.insert i q)
  pure q

sstatetoh :: Store -> SState a -> Handler a
sstatetoh store s = do
  x <- liftIO $ runReaderT (runExceptT s) store
  case x of
    Right k -> return $ k
    Left err -> throwError err

create :: Int -> SState Queue
create user = do
  newQ <- addTeamA 1 user emptyQueue
  do
    st <- ask
    liftIO $ modifyIORef st (M.insert 0 newQ) >> pure newQ
