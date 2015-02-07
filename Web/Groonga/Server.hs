{-# LANGUAGE OverloadedStrings #-}
module Web.Groonga.Server where

import Web.Scotty
import Data.Monoid (mconcat)
import Bindings.Groonga.Raw (C'_grn_ctx)
import qualified Bindings.Groonga.CommandAPI as Groonga
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class
import Foreign.Ptr

db :: String -> IO (Ptr C'_grn_ctx)
db dbpath = do
  ctx <- Groonga.grn_ctx_init
  Groonga.grn_database_create ctx dbpath
  return ctx

app :: Ptr C'_grn_ctx -> String -> ScottyM ()
app ctx dbpath = do
    get "/version" $ do
      ver <- get_groonga_version
      text $ mconcat ["{\"Groonga Version\": \"", ver, "\"}"]
      setHeader "Content-Type" "application/json; charset=utf-8"
    get "/d/:command" $ do
      command <- param "command"
      response <- send_groonga_command ctx (L.unpack command)
      text (L.pack response) -- just to send response. Don't decode with Aeson!
      setHeader "Content-Type" "application/json; charset=utf-8"

    where
      get_groonga_version :: ActionM L.Text
      get_groonga_version = liftIO $ do
        version <- Groonga.grn_get_version
        return (L.pack version)

      send_groonga_command :: Ptr C'_grn_ctx -> String -> ActionM String
      send_groonga_command ctx command = liftIO $ do
        _ <- Groonga.grn_database_open ctx dbpath
        response <- Groonga.grn_execute_command ctx command
        return response
