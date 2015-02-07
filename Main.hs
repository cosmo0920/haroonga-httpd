{-# LANGUAGE OverloadedStrings #-}
import Web.Groonga.Server
import Web.Scotty (scotty)

main :: IO ()
main = do
  let port = 3000
      dbpath = "db/test.db"

  ctx <- db dbpath
  scotty port $ app ctx dbpath
