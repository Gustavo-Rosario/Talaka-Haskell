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
import Handler.Utils
import qualified Prelude as P


getHomeR :: Handler Html
getHomeR = do
    logged <- isLogged
    defaultLayout $ do
        setTitle "Talaka Pocket - Plataforma de Financiamento para Quadrinhos"
        addStylesheet $ StaticR css_bootstrap_css
        case logged of
            Just 1 -> $(whamletFile "templates/indexUser.hamlet")
            Just 2 -> $(whamletFile "templates/indexAdmin.hamlet")
            Nothing -> $(whamletFile "templates/index.hamlet")
