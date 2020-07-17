{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Api
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( runStderrLoggingT )
import           Data.String.Conversions        ( cs )
import           Data.Text                      ( Text )
import           Database.Persist.Sqlite        ( Entity
                                                , (==.)
                                                , ConnectionPool
                                                , createSqlitePool
                                                , entityVal
                                                , runMigration
                                                , runSqlPersistMPool
                                                , runSqlPool
                                                , selectFirst
                                                , selectList
                                                , get
                                                )
import           Models
import           Network.Wai.Handler.Warp      as Warp
                                         hiding ( run )
import           Servant


server :: ConnectionPool -> Server Api
server pool = getAllItems :<|> getResource :<|> raw
 where
  getAllItems :: Handler [Text]
  getAllItems = liftIO $ flip runSqlPersistMPool pool $ do
    mMapping <- selectList [] []
    return $ map (itemName . entityVal) mMapping

  getResource :: Text -> Handler Text
  getResource name = do
    item <- liftIO $ flip runSqlPersistMPool pool $ do
      itemE     <- selectFirst [ItemName ==. name] []
      item      <- maybe (fail "not found") pure itemE
      resourceE <- get $ itemResource $ entityVal item
      resource  <- maybe (fail "not found") pure resourceE
      return $ resourceFilePath resource
    return item
    -- case item of
    --   Just i  -> return i
    --   Nothing -> throwError $ err404
    --     { errBody =
    --       "myfile.txt just isn't there, please leave this server alone."
    --     }
    -- where foo e = get $ itemResource $ entityVal e
    --join x = getEntity  $ itemResource $ entityVal x

  raw = serveDirectoryFileServer "static/"


app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile = do
  _app <- mkApp sqliteFile
  let settings = Warp.setPort 3000 $ Warp.setHost "*" $ Warp.defaultSettings
  Warp.runSettings settings _app


main :: IO ()
main = do
  putStrLn "Running!"
  run "data/sqlite.db"
