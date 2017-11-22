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

getAdminLoginR :: Handler Html
getAdminLoginR = do 
    defaultLayout $ do
        [whamlet|
            <h1 .naruto>
                Teste de Classe
            <h2 #sasuke>
                Teste de Id
        |]