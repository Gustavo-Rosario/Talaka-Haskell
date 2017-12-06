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
    case logged of
        Just 1 -> do
            (Just user) <- lookupSession "_USERID"
            Just (Entity userId _) <- runDB $ selectFirst [UserId ==. ( P.read . unpack $ user) ] []
            defaultLayout $ do
                setTitle "Talaka Pocket - Plataforma de Financiamento para Quadrinhos"
                addStylesheet $ StaticR css_bootstrap_css
                $(whamletFile "templates/indexUser.hamlet")
        Just 2 -> do
            defaultLayout $ do
                setTitle "Talaka Pocket - Plataforma de Financiamento para Quadrinhos"
                addStylesheet $ StaticR css_bootstrap_css
                $(whamletFile "templates/indexAdmin.hamlet")
        Nothing -> do
            defaultLayout $ do
                setTitle "Talaka Pocket - Plataforma de Financiamento para Quadrinhos"
                addStylesheet $ StaticR css_bootstrap_css
                $(whamletFile "templates/index.hamlet")