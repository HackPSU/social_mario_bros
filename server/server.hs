{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Monoid (mappend)
import Control.Exception (finally)
import Control.Monad (forM_, forever, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.Hashable (Hashable)
import Data.Char (isPunctuation, isSpace)
import qualified Data.Hashable as HT
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as FD

data Client = Client {
                       client_conn :: WS.Connection
                      ,client_name :: Text
                     }

instance Hashable Client where
  hashWithSalt salt (Client conn name) = HT.hashWithSalt salt name `mod` (maxBound :: Int)

type ServerState = HM.HashMap Text Client

newServerState :: ServerState
newServerState = HM.empty

numClients :: ServerState -> Int
numClients = HM.size

clientExists :: Client -> ServerState -> Bool
clientExists (Client _ name) state = maybe False (const True) $ HM.lookup name state

newClient :: Client -> ServerState -> ServerState
newClient client state = HM.insert (client_name client) client state

delClient :: Client -> ServerState -> ServerState
delClient client state = HM.delete (client_name client) state

broadcast :: Text -> ServerState -> IO ()
broadcast msg clients = FD.mapM_ (flip WS.sendTextData msg . client_conn) clients

validateName :: Text -> Bool
validateName name = not $ any ($ name) [T.null, T.any isPunctuation, T.any isSpace]

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9898 $ app state

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  -- Receive client unique nickname
  name <- WS.receiveData conn :: IO Text
  clients <- liftIO $ readMVar state
  let disconnect = do
          liftIO $ T.putStrLn $ name `mappend` " disconnected"
          modifyMVar_ state $ \s -> return $ delClient client s
      client = Client conn name
    in
    case name of
      _ | not (validateName name) -> WS.sendTextData conn ("Invalid name" :: Text)
        | clientExists client clients -> WS.sendTextData conn ("User already exists" :: Text)
        | otherwise -> flip finally disconnect $ do
                    liftIO $ modifyMVar_ state $ return.newClient client
                    liftIO $ T.putStrLn $ name `mappend` " logged in"
                    relay state client

relay :: MVar ServerState -> Client -> IO ()
relay state (Client conn _) = forever $ do
  msg <- WS.receiveData conn
  liftIO $ readMVar state >>= broadcast msg
