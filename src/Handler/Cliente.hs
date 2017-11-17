{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postClienteR :: Handler Value
postClienteR = do
    cliente <- requireJsonBody :: Handler Cliente
    cid <- runDB $ insert cliente
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey cid)])

getTodosClientesR :: Handler Value
getTodosClientesR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    sendStatusJSON ok200 (object ["resp" .= clientes])