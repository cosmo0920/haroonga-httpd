module Web.Groonga.Server where

import Web.Scotty
import Network.HTTP.Types
import Data.Monoid (mconcat)
import Bindings.Groonga.Raw (C'_grn_ctx)
import qualified Bindings.Groonga.CommandAPI as Groonga
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory
import Data.Time
import System.Locale
import Control.Applicative ((<$>))

type GrnCtx = Ptr C'_grn_ctx

db :: String -> IO ()
db dbpath = do
  ctx <- Groonga.grn_ctx_init
  create_db_if_needed ctx dbpath

create_db_if_needed :: GrnCtx -> String -> IO ()
create_db_if_needed ctx dbpath = do
  result <- doesFileExist dbpath
  if result
    then putStrLn $ "Skip create database. Already exists " ++ dbpath ++ "."
    else do
      _ <- Groonga.grn_database_create ctx dbpath
      return ()
  _ <- Groonga.grn_ctx_fin ctx
  return ()

app :: String -> ScottyM ()
app dbpath = do
    middleware logStdoutDev

    get "/version" $ do
      ver <- get_groonga_version
      text $ mconcat ["{\"Groonga Version\": \"", ver, "\"}"]
      set_json_header

    get "/d/:command" $ do
      command <- param "command"
      response <- send_groonga_command $ L.unpack command
      case response of
        Left res -> do
          text $ L.pack res
          status internalServerError500
          set_json_header
        Right res -> do
          text $ L.pack res
          set_json_header

    where
      get_groonga_version :: ActionM L.Text
      get_groonga_version = liftIO $ do
        version <- Groonga.grn_get_version
        return (L.pack version)

      send_groonga_command :: String -> ActionM (Either String String)
      send_groonga_command command = liftIO $ do
        ctx <- Groonga.grn_ctx_init
        _ <- Groonga.grn_database_open ctx dbpath
        start_at <- getCurrentTimeAsDouble
        response <- Groonga.grn_execute_command ctx command
        done_at <- getCurrentTimeAsDouble
        errbuf <- Groonga.grn_get_errbuf ctx
        _ <- Groonga.grn_ctx_fin ctx
        if length errbuf > 0
          then return $ Left $ format_err_response (-1) start_at done_at errbuf
          else return $ Right $ format_response 0 start_at done_at response

      set_json_header :: ActionM ()
      set_json_header = setHeader "Content-Type" "application/json; charset=utf-8"

      treat_as_string :: String -> String
      treat_as_string str = concat ["\"", str, "\""]

      getCurrentTimeAsDouble :: IO Double
      getCurrentTimeAsDouble = do
        epoch_double <- (read <$> formatTime defaultTimeLocale "%s.%q" <$> getCurrentTime) :: IO Double
        return epoch_double

      format_response :: Int -> Double -> Double -> String -> String
      format_response status start_at done_at response =
        concat ["[", "[", (show status), ",",
                          (show start_at), ",",
                          (show $ (done_at - start_at)), "],", response, "]"]

      format_err_response :: Int -> Double -> Double -> String -> String
      format_err_response status start_at done_at errbuf =
        concat ["[", "[", (show status), ",",
                          (show start_at), ",",
                          (show $ (done_at - start_at)), ",",
                          (treat_as_string errbuf), ",[]", "]]"]
