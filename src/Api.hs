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
  = "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
  :<|> Raw

api :: Proxy Api
api = Proxy
