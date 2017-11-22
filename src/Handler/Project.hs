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
import Handler.Form

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
        
getPerfilProjectR :: ProjectId -> Handler Html
getPerfilProjectR projectId = do
    projeto <- runDB $ get404 projectId
    usuario <- runDB $ get404 $ projectCreator projeto
    comentarios <-runDB $ selectList [CommentProject ==. projectId] [Desc CommentDateTime]
    comenuser <- sequence $ map (\ x -> (runDB $ get404 $ commentUser . entityVal $ x) >>= \y -> return (entityVal x,y)) comentarios
    (widget, enctype) <- generateFormPost $ formComment projectId 
    defaultLayout $ do
        [whamlet|
            <h1>
                Nome: #{projectTitle projeto}
            <h2>
                Criador: #{userName usuario}
            <h2>
                Data: #{show $ projectDateBegin projeto}
            <h2>
                Meta: #{projectMeta projeto}
            
            <form action=@{ComentarProjectR projectId} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Comentar">
                
            <h3>
                Comentários: 
            <ul>
                $forall (c, u) <- comenuser
                    <li>
                        #{userName u}: #{commentComment c}
            
        |]