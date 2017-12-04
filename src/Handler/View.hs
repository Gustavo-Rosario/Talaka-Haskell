{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.View where

import Import
import Text.Cassius
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Talaka Pocket - Plataforma de Financiamento para Quadrinhos"
        addStylesheet $ StaticR css_bootstrap_css
        -- $(whamletFile "templates/nav.hamlet")
        $(whamletFile "templates/index.hamlet")