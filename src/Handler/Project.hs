{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Project where

import Import
import Text.Cassius
import Database.Persist.Postgresql

formProject :: Form Project
formProject = renderDivs $ Project
    <$> areq textField "Título: " Nothing
    <*> areq textField "Descrição: " Nothing
    <*> areq intField "Meta: " Nothing
    <*> areq hiddenField "" (Just 0)
    <*> fmap utctDay (lift $ liftIO getCurrentTime) -- faz um IO funcionar em outra Monad
    <*> areq dayField "Prazo Final: " Nothing
    <*> pure (toSqlKey 1)

getCadProjR :: Handler Html
getCadProjR = do
    (widget, enctype) <- generateFormPost formProject
    defaultLayout $ do
        [whamlet|
            <h1>
                Cadastro de Projeto
            <form action=@{CadProjR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="ALOO">
        |]
postCadProjR :: Handler Html
postCadProjR = do
    ((result,_),_) <- runFormPost formProject
    case result of
        FormSuccess project -> do
            runDB $ insert project
            redirect CadProjR
        _ -> redirect HomeR        
        
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

getCadUserR :: Handler Html
getCadUserR = do
    (widget, enctype) <- generateFormPost formUser
    defaultLayout $ do
        [whamlet|
            <h1>
                Cadastro de Usuario
            <form action=@{CadUserR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar Usuário">
        |]

postCadUserR :: Handler Html
postCadUserR = do
    ((result,_),_) <- runFormPost formUser
    case result of
        FormSuccess user -> do
            runDB $ insert user
            redirect CadUserR
        _ -> redirect HomeR


getPerfilUserR :: UserId -> Handler Html
getPerfilUserR usuarioId = do
    usuario <- runDB $ get404 usuarioId
    defaultLayout $ do
        [whamlet|
            <h1>
                Nome: #{userName usuario}
            <h1>
                Login: #{userLogin usuario}
            <h1>
                Data: #{show $ userDateBirth usuario}
            
        |]
        
getPerfilProjectR :: ProjectId -> Handler Html
getPerfilProjectR projectId = do
    projeto <- runDB $ get404 projectId
    defaultLayout $ do
        [whamlet|
            <h1>
                Nome: #{projectTitle projeto}
            <h1>
                Criador: #{fromSqlKey $ projectCreator projeto}
            <h1>
                Data: #{show $ projectDateBegin projeto}
            <h1>
                Meta: #{projectMeta projeto}
            
        |]