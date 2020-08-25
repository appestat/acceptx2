{-# LANGUAGE OverloadedStrings #-}
module Match where
import Queue
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Network.Rcon.Client
import Servant.Docs
import qualified Data.ByteString.Char8 as B
import User

data Match = Match {
  team1 :: [Id],
  team2 :: [Id]
                   }

instance ToJSON Match where
  toJSON (Match{team1 = teamA, team2 = teamB}) = object ["team1" .= object ["players" .= toJSON (toJSON . show <$> teamA)], "team2" .= object ["players" .= toJSON ((toJSON . show) <$> teamA)]]

instance ToSample Match where
  toSamples _ = singleSample $ Match [0] [76561198217022140]

tellExecute :: (MonadIO m) => Int -> m ()
tellExecute x = do
  server <- liftIO $ readFile "server"
  pw <- liftIO $ B.readFile "password"
  handle <- liftIO $ connect server "27015" pw
  liftIO $ execCommand handle $ (B.pack $ "get5_loadmatch_url queue.boner1.com/" ++ show x ++ "/config")
  return ()
