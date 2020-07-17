{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import           Data.Text
import           Database.Persist.TH
import           Data.Time

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Item
  name  Text
  resource  ResourceId
  UniqueName name
  deriving Eq Read Show
Resource
  filePath  Text
  date UTCTime default=CURRENT_TIMESTAMP
  deriving Eq Read Show
|]
