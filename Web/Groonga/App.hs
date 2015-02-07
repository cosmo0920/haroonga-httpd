module Web.Groonga.App where

import Web.Groonga.Server
import Web.Scotty (scotty)
import Options.Applicative

data Config = Config
  { port   :: Int
  , dbpath :: String }

config :: Parser Config
config = Config
     <$> option auto
         ( long "port"
        <> metavar "NUMBER"
        <> value 3000
        <> help "Specify port number" )
     <*> strOption
         ( long "dbpath"
        <> metavar "DBPATH"
        <> help "Specify database path" )

server :: Config -> IO ()
server (Config port dbpath) = do
  ctx <- db dbpath
  scotty port $ app ctx dbpath

defaultMain :: IO ()
defaultMain = do
  execParser opts >>= server
  where
    opts = info (helper <*> config)
           ( fullDesc
             <> progDesc "haroonga http server."
             <> header "Haroonga http server" )
