{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import           Api
import           Control.Monad.Logger           ( runStderrLoggingT )
import           Data.String.Conversions        ( cs )
import           Data.Text                      ( Text, unpack )
import           Data.Text.IO                   ( hGetContents )
import           Database.Esqueleto
import qualified Database.Persist.Sqlite       as P
import           Models
import           Network.Wai.Handler.Warp      as Warp
                                         hiding ( run )
import           Control.Monad.Except
import           Servant
import           Control.Monad.Reader           ( ReaderT )
import qualified Data.Maybe                    as M
import           Data.Time.Clock
import           System.IO                      ( openFile
                                                , IOMode(ReadMode)
                                                , hClose
                                                )


server :: ConnectionPool -> Server Api
server pool = getAllItems :<|> getResource :<|> raw
 where
  getAllItems :: Handler [Text]
  getAllItems = liftIO $ flip runSqlPersistMPool pool $ do
    names <- select $ from $ \items -> do
      return items
    return $ (itemName . entityVal) <$> names

  getResource :: Text -> Handler Text
  getResource name = do
    x <- getResourcePath name
    case x of
      Just path -> liftIO $ do
        handle   <- openFile (unpack path) ReadMode
        contents <- hGetContents handle
        hClose handle
        return contents
      Nothing -> throwError $ err404 { errBody = "Could not find item" }

  getResourcePath :: (MonadIO m) => Text -> m (Maybe Text)
  getResourcePath name = liftIO $ flip runSqlPersistMPool pool $ do
    resource <- select $ from $ \(items, resources) -> do
      where_
        (   (items ^. ItemResource ==. resources ^. ResourceId)
        &&. (items ^. ItemName ==. val name)
        )
      limit 1
      return resources
    return $ M.listToMaybe $ (resourceFilePath . entityVal) <$> resource

  raw = serveDirectoryFileServer "static/"

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    P.createSqlitePool (cs sqliteFile) 5

  flip runSqlPool pool $ do
    runMigration migrateAll
    seed
  return $ app pool

seed :: ReaderT SqlBackend IO ()
seed = do
  [Value x] <- select $ from $ \(_ :: SqlExpr (Entity Item)) -> do
    return countRows
  if (x :: Int) == 0
    then do
      time <- liftIO getCurrentTime
      r    <- insert $ Resource "/workspaces/rapt/TODO.md" time
      insert_ $ Item "foo" r
    else return ()

runApp :: FilePath -> IO ()
runApp sqliteFile = do
  _app <- mkApp sqliteFile
  let settings = Warp.setPort 3000 $ Warp.setHost "*" $ Warp.defaultSettings
  Warp.runSettings settings _app


main :: IO ()
main = do
  putStrLn "Running!"
  runApp "data/sqlite.db"
