{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.Proxy
import           Data.Text
import           Models
import           Servant.API


-- brittany-disable-next-binding
type Api
  = "item" :> Get '[JSON] [Text]
  :<|> "item" :> Capture "name" Text  :> Get  '[JSON] Text
  :<|> Raw

api :: Proxy Api
api = Proxy
