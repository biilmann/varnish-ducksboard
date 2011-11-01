{-# LANGUAGE OverloadedStrings #-}
module Main where


import System.IO
import System.Posix.Env
import System.Posix.Time
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Enumerator
import Network.TLS (TLSCertificateUsage(..))


main = do
  Just key <- getEnv "DB_KEY"
  Just wid <- getEnv "DB_WIDGET_ID"
  requests <- newMVar 0
  forkIO $ logger key wid requests
  hGetContents stdin >>= mapM_ (process requests) . lines


process requests line =
  case drop 1 (words line) of
    ("ReqEnd":xs) -> modifyMVar_ requests $ \counter -> return (counter + 1)
    __            -> return ()


logger key wid requests = forever $ do
    threadDelay (60000000)
    modifyMVar_ requests $ \counter -> do
      logRequests key wid counter
      return 0


logRequests key wid counter = do
    time    <- epochTime
    request <- apiRequest time
    withManager $ \manager -> httpLbs request manager
  where
    apiRequest time = do
      req <- parseUrl url
      return $ withAuth req {
        requestBody = RequestBodyBS $ B.pack (body time),
        checkCerts = (\_ _ -> return CertificateUsageAccept),
        method = "POST"
      }
    url = "https://push.ducksboard.com/values/" ++ wid ++ "/"
    body time = "{\"timestamp\":" ++ (show time) ++ ",\"value\":" ++ (show counter) ++ "}"
    withAuth = applyBasicAuth (B.pack key) "ignore"
