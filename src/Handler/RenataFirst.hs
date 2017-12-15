{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.RenataFirst where

import Import
import Database.Persist.Postgresql(toSqlKey, fromSqlKey)
import qualified Network.Wreq as NW

-- import Network.Wreq.Session
-- Operators such as (&) and (.~).
import Control.Lens((.~),(&), (^.), (^?))
-- foi da documentacao q tu me passou
-- Conversion of Haskell values to JSON.
import Data.Aeson (toJSON, fromJSON)
import Data.Maybe
-- -- Easy traversal of JSON data.
import Data.Aeson.Lens as A
import Data.ByteString.Lazy.Internal
-- import Network.HTTP.Types as Import
--     ( status200
--     , status201
--     , status400
--     , status403
--     , status404
--     )

getProjectR :: ProjectId -> Handler Value
getProjectR pid = do
    project <- runDB $ get404 pid
    sendResponseStatus status200 $ object ["project" .= project]

-- getProjectTesteR :: Handler Value
-- getProjectTesteR = undefined
-- -- do
-- --     r <- NW.get "https://haskmu-romefeller.c9users.io/project/13"
-- --     let resp =  r ^. responseBody
--     -- sendResponseStatus status200 $ object ["project" .= "success"]
    

-- getJson = do
--     r <- NW.get "viacep.com.br/ws/01001000/json"
--     return $ r ^? responseBody . (A.key "logradouro")