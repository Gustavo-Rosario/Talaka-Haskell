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
    deleteSession "_USER"
    redirect HomeR

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