{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Serie where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postSerieR :: Handler Value
postSerieR = do
    serie <- requireJsonBody :: Handler Serie
    serieid <- runDB $ insert serie
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey serieid)])

getConsultaSerieR :: SerieId -> Handler Value
getConsultaSerieR serieid = do
    serie <- runDB $ get404 serieid
    sendStatusJSON ok200 (object ["resp" .= serie])

getTodasSeriesR :: Handler Value
getTodasSeriesR = do
    series <- runDB $ selectList [] [Asc SerieNome]
    sendStatusJSON ok200 (object ["resp" .= series])
    
deleteApagarSerieR :: SerieId -> Handler Value
deleteApagarSerieR serieid = do
    _ <- runDB $ get404 serieid
    runDB $ delete serieid
    sendStatusJSON noContent204 (object ["resp" .= serieid])

putAlterarSerieR :: SerieId -> Handler Value
putAlterarSerieR serieid = do
    _ <- runDB $ get404 serieid
    novaSerie <- requireJsonBody :: Handler Serie
    runDB $ replace serieid novaSerie
    sendStatusJSON noContent204 (object ["resp" .= serieid])

patchAlterarNomeR :: SerieId -> Text -> Handler Value
patchAlterarNomeR serieid nome = do
    _ <- runDB $ get404 serieid
    runDB $ update serieid [SerieNome =. nome]
    sendStatusJSON noContent204 (object ["resp" .= serieid])