{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Text.Cassius
import Database.Persist.Postgresql
import Handler.Form
        
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    defaultLayout $ do 
        setTitle "Talaka ◆ Pocket - Login"
        $(whamletFile "templates/login.hamlet")

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess ("talakapocket","talaka") -> do
            setSession "_ADMIN" (pack (show $ Admin "admin" "talakapocket" "")) -- "Objeto de Admin"
                                -- Para mostrar o objeto, temos que transformar em String, utilizando o Show.
                                -- Só que temos um probleminha: Diferença entre String e Text!
                                -- Precisamos de String, então o PACK faz a conversão String -> Text.
            redirect AdminR
        FormSuccess (login, pwd) -> do
            mUser <-isAuth login pwd
            case mUser of
                Nothing -> do
                    setMessage [shamlet|
                        <h1>
                            Usuario não cadastrado / Senha inválida
                    |]
                    redirect LoginR
                Just(Entity userId (User name login email _ bio img cover date)) -> do
                    setSession "_USER" (pack (show $ User name login email "" bio img cover date))
                    setSession "_USERID" (pack (show userId))
                    redirect (PerfilUserR userId)
        _ -> do
            setMessage [shamlet|
                <h1>
                    SAFADO | PERANHA
            |]
            redirect LoginR
-- BD(Maybe(Enity Usuario))        
isAuth :: Text -> Text -> HandlerT App IO (Maybe (Entity User)) -- Trocando a Monada
isAuth login senha = runDB $ selectFirst [UserLogin ==. login, UserPwd ==. senha] []


postLogoutR :: Handler Html
postLogoutR = do
    session <- lookupSession "_USERID"
    case session of
        Just _ -> do
            deleteSession "_USER"
            deleteSession "_USERID"
        Nothing -> do
            deleteSession "_ADMIN"
    redirect HomeR