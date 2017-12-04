{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Text.Cassius
import Database.Persist.Postgresql
import Handler.Form

getAdminR :: Handler Html
getAdminR = do 
    defaultLayout $ do
        setTitle "Talaka - Perfil de Administrador"
        $(whamletFile "templates/home.hamlet")
        [whamlet|
            <h1 .naruto>
                Teste de Classe
            <h2 #sasuke>
                Teste de Id
        |]