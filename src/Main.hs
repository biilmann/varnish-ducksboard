{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where


import System.IO
import System.Posix.Time
import System.Posix.Types (EpochTime)
import qualified System.Console.CmdArgs.Implicit as Arg
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Data
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap as M
import Text.Printf
import Network.HTTP.Enumerator
import Network.TLS (TLSCertificateUsage(..))


data Config = Config
            { api_key         :: String
            , requests_widget :: Maybe String
            , errors_widget   :: Maybe String
            , domains_widget  :: Maybe String
            } deriving (Show, Data, Typeable)


config = Config { api_key = Arg.def, requests_widget = Arg.def, errors_widget = Arg.def, domains_widget = Arg.def }

main = do
    args <- Arg.cmdArgs config

    let key = api_key config

    requests <- newMVar 0
    errors   <- newMVar (0,0)
    domains  <- newMVar M.empty

    forkIO $ requestLogger key (requests_widget args) requests
    forkIO $ errorLogger   key (errors_widget args)   errors
    forkIO $ domainLogger  key (domains_widget args)  domains

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


widgetLogger _ _ _ _ Nothing _ = putStrLn "Nothing to do"
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
