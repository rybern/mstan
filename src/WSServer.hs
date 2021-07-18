{-# LANGUAGE OverloadedStrings #-}
module WSServer where
import           Network.WebSockets
import qualified Data.Text                     as Text
import qualified Data.ByteString.Lazy.Char8          as BSL

port = 12345

runServer :: (Text.Text -> IO Text.Text) -> IO ()
runServer go = do
  putStrLn $ "Listening on port " ++ show port
  runServerWithOptions
    (defaultServerOptions {
        serverHost = "0.0.0.0"
        , serverPort = port
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
