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

formProject :: UserId -> Form Project
formProject userid = renderDivs $ Project
    <$> areq textField "Título: " Nothing
    <*> areq textField "Descrição: " Nothing
    <*> areq intField "Meta: " Nothing
    <*> areq hiddenField "" (Just 0)
    <*> fmap utctDay (lift $ liftIO getCurrentTime) -- faz um IO funcionar em outra Monad
    <*> areq dayField "Prazo Final: " Nothing
    <*> pure userid
    <*> aopt hiddenField "" Nothing
    <*> aopt hiddenField "" Nothing
    <*> areq hiddenField "" (Just 0)
    
formProjectImg :: Form (FileInfo, FileInfo)
formProjectImg = renderDivs $ (,)
    <$> areq fileField FieldSettings{fsId=Just "hident1",
                                    fsLabel="Foto de Destaque: ",
                                    fsTooltip= Nothing,
                                    fsName= Nothing,
                                    fsAttrs=[("accept","image/*")]} Nothing
    <*> areq fileField FieldSettings{fsId=Just "hident1",
                                    fsLabel="Foto de Capa: ",
                                    fsTooltip= Nothing,
                                    fsName= Nothing,
                                    fsAttrs=[("accept","image/*")]} Nothing
-- CadUser   
formUser :: Form User
formUser = renderDivs $ User
    <$> areq textField "Nome do usuário: " Nothing
    <*> areq textField "Login: " Nothing
    <*> areq emailField "Email: " Nothing
    <*> areq passwordField "Senha: " Nothing
    <*> aopt textareaField "Biografia: " Nothing
    <*> aopt hiddenField "" Nothing
    <*> aopt hiddenField "" Nothing
    <*> areq dayField "Data de Nascimento: " Nothing
formImgs :: Form (FileInfo, FileInfo)    
formImgs = renderDivs $ (,) 
    <$> areq fileField FieldSettings{fsId=Just "hident1",
                                    fsLabel="Foto de Perfil: ",
                                    fsTooltip= Nothing,
                                    fsName= Nothing,
                                    fsAttrs=[("accept","image/*")]} Nothing
    <*> areq fileField FieldSettings{fsId=Just "hident1",
                                    fsLabel="Foto de Capa: ",
                                    fsTooltip= Nothing,
                                    fsName= Nothing,
                                    fsAttrs=[("accept","image/*")]} Nothing
-- LOGIN
formLogin :: Form(Text, Text)
formLogin = renderDivs $ (,)
    <$> areq textField "Email: " Nothing
    <*> areq passwordField "Senha: " Nothing
    
-- BUSCA 
formSearch :: Form(Text, Text)
formSearch = renderDivs $ (,)
    <$> areq textField "Busca" Nothing
    <*> areq hiddenField "" (Just "alo")

