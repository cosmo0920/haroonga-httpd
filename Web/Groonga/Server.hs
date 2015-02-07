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
    get "/d/version" $ do
      ver <- getGroongaVersion
      html $ mconcat ["<h1>Groonga Version: ", ver]
    get "/d/:command" $ do
      command <- param "command"
      response <- sendGroongaCommand ctx (L.unpack command)
      text (L.pack response) -- just to send response. Don't decode with Aeson!
      setHeader "Content-Type" "application/json; charset=utf-8"

    where
      getGroongaVersion :: ActionM L.Text
      getGroongaVersion = liftIO $ do
        version <- Groonga.grn_get_version
        return (L.pack version)

      sendGroongaCommand :: Ptr C'_grn_ctx -> String -> ActionM String
      sendGroongaCommand ctx command = liftIO $ do
        _ <- Groonga.grn_database_open ctx dbpath
        response <- Groonga.grn_execute_command ctx command
        return response
