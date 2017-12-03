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
        
-- BD(Maybe(Enity Usuario))        
-- autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity User))
-- autenticar email senha = runDB $ selectFirst [UserEmail ==. email
--                                              ,UserPwd ==. senha] []










-- postLoginR :: Handler Html
-- postLoginR = do
--     ((res,_),_) <- runFormPost formLogin
--     case res of
--         FormSuccess ("root@root.com","root") -> do 
--             setSession "_USR" (pack (show $ Adm "admin" "" "" ""))
--             redirect AdminR
--         FormSuccess (email,senha) -> do
--             talvezUsuario <- autenticar email senha
--             case talvezUsuario of
--                 Nothing -> do
--                     setMessage [shamlet|
--                         <h1>
--                             Usuario nao cadastrado/Senha invalida
--                     |]
--                     redirect LoginR
--                 Just (Entity uid (User n l e _ b i co d)) -> do
--                     setSession "_USR" (pack (show $ User n l e "" b i co d))
--                     redirect (PerfilUserR uid)
--         _ -> redirect HomeR
                        
                        
-- postLogoutR :: Handler Html
-- postLogoutR = do 
--     deleteSession "_USR"
--     redirect HomeR

-- getAdminR :: Handler Html 
-- getAdminR = do 
--     defaultLayout [whamlet|
--         <h1> BEM VINDO ADMIN!
--         <form action=@{LogoutR} method=post>
--             <input type="submit" value="Logout">
--     |]