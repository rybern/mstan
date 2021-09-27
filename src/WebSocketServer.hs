{-# LANGUAGE OverloadedStrings #-}
module WebSocketServer where
import           Network.WebSockets
import qualified Data.Text                     as Text
import qualified Data.ByteString.Lazy.Char8          as BSL


data WSServerOptions = WSServerOptions {
    port :: Int
  }
  deriving Show

defaultWSServerOptions = WSServerOptions {
    port = 12345
  }

runWSServer :: WSServerOptions -> (Text.Text -> IO Text.Text) -> IO ()
runWSServer options go = do
  putStrLn $ "Listening on port " ++ show (port options)
  runServerWithOptions
    (defaultServerOptions {
        serverHost = "0.0.0.0"
        , serverPort = port options
        })
    (\pending -> do
        print "GOT CONNECTION"
        conn <- acceptRequest pending
        print "ACCEPTED"
        (Text msg _) <- receiveDataMessage conn
        putStrLn $ "msg:"
        print $ msg
        reply <- go . Text.pack . BSL.unpack $ msg
        putStrLn $ "reply:"
        print $ reply
        sendDataMessage conn $ Text (BSL.pack . Text.unpack $ reply) Nothing
    )
