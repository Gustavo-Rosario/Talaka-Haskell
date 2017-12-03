{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.User where

import Import
import Text.Cassius
import Database.Persist.Postgresql
import Handler.Form

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
        
            <div .areah1>
                <h1>
                    Nome: #{userName usuario}
                    
            <h1>
                Login: #{userLogin usuario}
            <h1>
                Data: #{show $ userDateBirth usuario}
        |]