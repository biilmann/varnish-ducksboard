{-# LANGUAGE OverloadedStrings #-}
module Main where


import System.IO
import System.Posix.Env
import System.Posix.Time
import System.Posix.Types (EpochTime)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap as M
import Text.Printf
import Network.HTTP.Enumerator
import Network.TLS (TLSCertificateUsage(..))


main = do
  Just key <- getEnv "DB_KEY"
  rid <- getEnv "DB_REQ_MIN_ID"
  eid <- getEnv "DB_ERROR_ID"
  did <- getEnv "DB_DOMAIN_ID"

  requests <- newMVar 0
  errors   <- newMVar (0,0)
  domains  <- newMVar M.empty

  forkIO $ requestLogger key rid requests
  forkIO $ errorLogger key eid errors
  forkIO $ domainLogger key did domains

  hGetContents stdin >>= mapM_ (process requests errors domains) . lines


process requests errors domains line =
    case drop 1 (words line) of
      ("ReqEnd":xs)   -> modifyMVar_ requests $ \counter -> return (counter + 1)
      ("RxStatus":xs) -> modifyMVar_ errors   $ \(bad, total) ->
          return $ if success xs then (bad, total + 1) else (bad + 1, total + 1)
      ("RxHeader":"c":"Host:":domain:_) -> modifyMVar_ domains $ \dom ->
          return $ M.alter updateDomain domain dom
      __              -> return ()
  where
    success (_:s:_) =
        let status = read s in
        status == 404 || (status >= 200 && status <= 400)
    updateDomain Nothing = Just 1
    updateDomain (Just count) = Just (count + 1)


requestLogger = widgetLogger 60000000 0 requestWidgetBody


errorLogger = widgetLogger 60000000 (0,0) errorWidgetBody


domainLogger = widgetLogger (5 * 60000000) M.empty domainWidgetBody


widgetLogger _ _ _ _ Nothing _ = return ()
widgetLogger interval zero bodyFn key (Just wid) mvar = forever $ do
    threadDelay interval
    modifyMVar_ mvar $ \counter -> do
      time    <- epochTime
      updateWidget key wid (bodyFn time counter)
      return zero


requestWidgetBody _ counter = "{\"value\":" ++ (show counter) ++ "}"


errorWidgetBody :: EpochTime -> (Double, Double) -> String
errorWidgetBody _ (bad, total) = "{\"value\":" ++ (printf "%.2f" $ bad / total) ++ "}"


domainWidgetBody time map =
    let (domain, hits) = M.foldWithKey mostPopular ("", 0) map in
    "{\"timestamp\":" ++ (show time) ++ "," ++
    "\"value\": {\"title\":\"" ++ domain ++ "\", " ++
    "\"image\":\"https://app.ducksboard.com/static/img/timeline/green.gif\"," ++
    "\"content\":\"" ++ (show hits) ++ " hits\"," ++
    "\"link\":\"http://" ++ domain ++ "\"}}"
  where
    mostPopular domain hits (topDomain, topHits) = if hits > topHits then (domain, hits) else (topDomain, topHits)


updateWidget key wid body = do
    request <- apiRequest key wid body
    putStrLn $ "Sending: " ++ body
    withManager $ \manager -> do
        response <- httpLbs request manager
        putStrLn (show response)


apiRequest key wid body = do
    req <- parseUrl url
    return $ withAuth req {
      requestBody = RequestBodyBS (B.pack body),
      checkCerts = (\_ _ -> return CertificateUsageAccept),
      method = "POST"
    }
  where
    url = "https://push.ducksboard.com/values/" ++ wid ++ "/"
    withAuth = applyBasicAuth (B.pack key) "ignore"
