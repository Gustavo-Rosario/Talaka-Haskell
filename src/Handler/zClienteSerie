{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ClienteSerie where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postAcompanharR :: SerieId -> ClienteId -> Handler Value
postAcompanharR serieid cid = do
    csid <- runDB $ insert (ClienteSerie serieid cid)
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey csid)])

third :: (a,b,c) -> c
third (_,_,c) = c

-- Entity Serie = Entity SerieId Serie
getExpectadoresR :: SerieId -> Handler Value
getExpectadoresR serieid = do 
    lista <- runDB $ rawSql
        ("Select ??, ??, ?? \
    \FROM serie INNER JOIN cliente_serie \
    \ON serie.id=cliente_serie.serieid INNER JOIN cliente \
    \ON cliente_serie.usrid = cliente.id WHERE serie.id = " <> (pack $ show $ fromSqlKey serieid))
        [] :: Handler [(Entity Serie, Entity ClienteSerie, Entity Cliente)]
    listaCliente <- return $ map third lista
    listaClienteSohCliente <- return $ map entityVal listaCliente
    sendStatusJSON ok200 (object ["resp" .= listaClienteSohCliente])

getExpectadoresFR :: SerieId -> Handler Value
getExpectadoresFR serieid = do
    listaEntityClienteSerie <- runDB $ selectList [ClienteSerieSerieid ==. serieid] []
    listaClienteSerie <- return $ map entityVal listaEntityClienteSerie
    idsCliente <- return $ map clienteSerieSerieid listaClienteSerie
    clientes <- sequence $ map (\ x -> runDB $ get404 x ) idsCliente -- Sequence -> remove os handlers
    sendStatusJSON ok200 (object ["resp" .= clientes])