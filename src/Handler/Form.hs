{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Form where

import Import
import Database.Persist.Postgresql
-- FORMULARIOS
formComment :: ProjectId -> Form Comment
formComment pId = renderDivs $ Comment
    <$> areq textareaField "Comentario: " Nothing
    <*> pure(toSqlKey 1)
    <*> pure(pId)
    <*> lift( liftIO getCurrentTime)
    <*> areq hiddenField "" (Just 0)

formProject :: Form Project
formProject = renderDivs $ Project
    <$> areq textField "Título: " Nothing
    <*> areq textField "Descrição: " Nothing
    <*> areq intField "Meta: " Nothing
    <*> areq hiddenField "" (Just 0)
    <*> fmap utctDay (lift $ liftIO getCurrentTime) -- faz um IO funcionar em outra Monad
    <*> areq dayField "Prazo Final: " Nothing
    <*> pure (toSqlKey 1)
    
formUser :: Form User
formUser = renderDivs $ User
    <$> areq textField "Nome do usuário: " Nothing
    <*> areq textField "Login: " Nothing
    <*> areq emailField "Email: " Nothing
    <*> areq passwordField "Senha: " Nothing
    <*> aopt textareaField "Biografia: " Nothing
    <*> aopt hiddenField "Foto de Perfil: " Nothing
    <*> aopt hiddenField "Foto de Capa: " Nothing
    <*> areq dayField "Data de Nascimento: " Nothing
