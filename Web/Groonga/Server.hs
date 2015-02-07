{-# LANGUAGE OverloadedStrings #-}
module Web.Groonga.Server where

import Web.Scotty
import Data.Monoid (mconcat)
import Bindings.Groonga.Raw (C'_grn_ctx)
import qualified Bindings.Groonga.CommandAPI as Groonga
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class
import Foreign.Ptr
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

type GrnCtx = Ptr C'_grn_ctx

db :: String -> IO GrnCtx
db dbpath = do
  ctx <- Groonga.grn_ctx_init
  Groonga.grn_database_create ctx dbpath
  return ctx

app :: GrnCtx -> String -> ScottyM ()
app ctx dbpath = do
    middleware logStdoutDev
    get "/version" $ do
      ver <- get_groonga_version
      text $ mconcat ["{\"Groonga Version\": \"", ver, "\"}"]
      set_json_header
    get "/d/:command" $ do
      command <- param "command"
      response <- send_groonga_command ctx (L.unpack command)
      text (L.pack response) -- just to send response. Don't decode with Aeson!
      set_json_header

    where
      get_groonga_version :: ActionM L.Text
      get_groonga_version = liftIO $ do
        version <- Groonga.grn_get_version
        return (L.pack version)

      send_groonga_command :: GrnCtx -> String -> ActionM String
      send_groonga_command ctx command = liftIO $ do
        _ <- Groonga.grn_database_open ctx dbpath
        response <- Groonga.grn_execute_command ctx command
        return response

      set_json_header :: ActionM ()
      set_json_header = setHeader "Content-Type" "application/json; charset=utf-8"
