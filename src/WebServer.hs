{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module WebServer where

import           Control.Concurrent
import           Network.Simple.TCP
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Parsing
import           Debug.Trace

data WebServerOptions = WebServerOptions {
    port :: String
  }

defaultWebServerOptions = WebServerOptions {
    port = "12345"
  }

readText :: Socket -> IO (Maybe Text)
readText socket = ((Text.pack . BS.unpack) <$>) <$> recv socket 100000000

sendText :: Socket -> Text -> IO ()
sendText socket = send socket . BS.pack . Text.unpack

unfoldRead :: IO (Maybe Text) -> IO Text
unfoldRead readText = readText >>= \line -> do
    print line
    case Text.strip <$> line of
        Nothing      -> return ""
        Just "<end>" -> return ""
        Just msg     -> do
            rest <- unfoldRead readText
            return $ msg <> rest

runServer :: WebServerOptions -> (IO (Maybe Text) -> IO [Text]) -> IO ()
runServer options go = do
    serve (Host "127.0.0.1") (port options) $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        res <- go (readText connectionSocket)
        mapM_ (sendText connectionSocket) res
        print res
        closeSock connectionSocket

    -- let while = do
    --       putStrLn $ "listen"
    --       recv connectionSocket 100000000 >>= print
    --       threadDelay 1000000
    --       while
    -- while
    -- return ()
