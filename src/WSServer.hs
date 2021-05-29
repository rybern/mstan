{-# LANGUAGE OverloadedStrings #-}
module WSServer where
import           Network.WebSockets
import qualified Data.Text                     as Text
import qualified Data.ByteString.Lazy.Char8          as BSL

runServer :: (Text.Text -> IO Text.Text) -> IO ()
runServer go = runServerWithOptions
  (defaultServerOptions {
      serverHost = "127.0.0.1"
      , serverPort = 12345
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
