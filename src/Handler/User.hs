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
import Data.Maybe
import Handler.Utils
import qualified Prelude as P

getCadUserR :: Handler Html
getCadUserR = do
    (widget, enctype) <- generateFormPost formUser
    defaultLayout $ do
        setTitle "Talaka Pocket - Cadastro de Usuário"
        $(whamletFile "templates/caduser.hamlet")

postCadUserR :: Handler Html
postCadUserR = do
    ((result,_),_) <- runFormPost formUser
    case result of
        FormSuccess user -> do
            userid <- runDB $ insert user
            redirect (CadUserImgsR userid)
        _ -> redirect HomeR
        
getCadUserImgsR :: UserId -> Handler Html
getCadUserImgsR userid = do
    (widget, enctype) <- generateFormPost formImgs
    defaultLayout $ do
        setTitle "Talaka Pocket - Imagens Perfil"
        $(whamletFile "templates/img.hamlet")
        
postCadUserImgsR :: UserId -> Handler Html
postCadUserImgsR userid = do
    _ <- runDB $ get404 userid
    ((result,_),_) <- runFormPost formImgs
    case result of
        FormSuccess (perfil, cover) -> do
            liftIO $ fileMove perfil ("static/img/users/" ++ (unpack $ fileName perfil))
            liftIO $ fileMove cover ("static/img/covers/" ++ (unpack $ fileName cover))
            runDB $ update userid [UserImg =. (Just (fileName perfil)), UserCover =. (Just (fileName cover))]
            (User name login email _ bio img cover date) <- runDB $ get404 userid
            setSession "_USER" (pack(show $ User name login email "" bio img cover date))
            setSession "_USERID" (pack(show $ fromSqlKey userid))
            redirect (PerfilUserR userid) 
        _ -> do
            setMessage [shamlet|
                <h1>
                    Problema em imagens
            |]
            redirect (CadUserImgsR userid)

getPerfilUserR :: UserId -> Handler Html
getPerfilUserR userid = do
    (logged,_) <- isLogged
    user <- runDB $ get404 userid
    userImg <- return $ StaticRoute ["img","users", fromJust(userImg user)] []
    userCover <- return $ StaticRoute ["img","covers", fromJust(userCover user)] []
    userProjs <- runDB $ selectList [ProjectCreator ==. userid] [Desc ProjectId]
    finances <- selectDiferentProject userid
    userFins <- sequence $ map (\(Entity fid fin) -> (runDB $ get404 (financingProject fin)) >>= \project -> (runDB $ get404 (projectCreator project) )>>= \creator -> return (project, creator, (financingProject fin)) ) finances
    -- Criar a imagem da fora para poder usar dentro do template.
    -- o Static Route é uma coisa pura, e como estamos dentro da Handler, temos que trocar.
    defaultLayout $ do
        setTitle "Talaka Pocket - Perfil de Usuário"
        $(whamletFile "templates/nav.hamlet")
        $(whamletFile "templates/perfil.hamlet")
        $(whamletFile "templates/footer.hamlet")

getMeuPerfilR :: Handler Html
getMeuPerfilR = do
    (logged,_) <- isLogged
    (Just uid) <- lookupSession "_USERID"
    (Just u) <- lookupSession "_USER"
    user <- return $ (P.read . unpack $ u)  :: Handler User
    userId <- return $ (toSqlKey . P.read . unpack $ uid) :: Handler UserId
    userImg <- return $ StaticRoute ["img","users", fromJust(userImg user)] []
    userCover <- return $ StaticRoute ["img","covers", fromJust(userCover user)] []
    userProjs <- runDB $ selectList [ProjectCreator ==. userId] [Desc ProjectId]
    finances <- selectDistinctProject userId
    userFins <- sequence $ map (\fin -> (runDB $ get404 fin) >>= \project -> (runDB $ get404 (projectCreator project)) >>= \creator -> return (project, creator, fin) ) finances
    -- Criar a imagem da fora para poder usar dentro do template.
    -- o Static Route é uma coisa pura, e como estamos dentro da Handler, temos que trocar.
    defaultLayout $ do
        setTitle "Talaka Pocket - Perfil de Usuário"
        $(whamletFile "templates/nav.hamlet")
        $(whamletFile "templates/perfil.hamlet")
        $(whamletFile "templates/footer.hamlet")        

selectDiferentProject :: UserId -> Handler [Entity Financing]
selectDiferentProject uid = runDB $ rawSql "SELECT ?? FROM financing WHERE financing.user = ?  GROUP BY financing.project, financing.id" [ toPersistValue uid ]
-- DISTINCT
selectDistinctProject :: UserId -> Handler [ProjectId]
selectDistinctProject uid = runDB $ rawSql "SELECT DISTINCT( financing.project ) FROM financing WHERE financing.user = ?  ORDER BY financing.project DESC" [ toPersistValue uid ]