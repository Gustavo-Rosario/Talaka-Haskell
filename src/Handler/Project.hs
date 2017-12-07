{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Project where

import Import
import Text.Cassius
import Database.Persist.Postgresql
import Handler.Form
import Data.Maybe
import Handler.Utils
import qualified Prelude as P

getCadProjR :: Handler Html
getCadProjR = do
    (logged, mUser) <- isLogged
    (Just user) <- lookupSession "_USERID"
    Just (Entity userId _) <- runDB $ selectFirst [UserId ==. ( P.read . unpack $ user) ] []
    (widget, enctype) <- generateFormPost (formProject userId)
    defaultLayout $ do
        setTitle "Talaka Pocket - Cadastro Projeto"
        $(whamletFile "templates/nav.hamlet")
        $(whamletFile "templates/cadproj.hamlet")
        
postCadProjR :: Handler Html
postCadProjR = do
    (Just user) <- lookupSession "_USERID"
    Just (Entity userId _) <- runDB $ selectFirst [UserId ==. ( P.read . unpack $ user) ] []
    ((result,_),_) <- runFormPost (formProject userId) 
    case result of
        FormSuccess project -> do
            projectid <- runDB $ insert project
            redirect (CadProjImgsR projectid)
        _ -> redirect HomeR 

getCadProjImgsR :: ProjectId -> Handler Html
getCadProjImgsR projectid = do 
    (widget, enctype) <- generateFormPost formProjectImg
    defaultLayout $ do
        setTitle "Talaka Pocket - Imagens do Projeto"
        $(whamletFile "templates/projImg.hamlet")
        
postCadProjImgsR :: ProjectId -> Handler Html
postCadProjImgsR projectid = do
    _ <- runDB $ get404 projectid
    ((result,_),_) <- runFormPost formProjectImg
    case result of
        FormSuccess (destaque, cover) -> do
            liftIO $ fileMove destaque ("static/img/proj/" ++ (unpack $ fileName destaque))
            liftIO $ fileMove cover ("static/img/proj/" ++ (unpack $ fileName cover))
            runDB $ update projectid [ProjectDes =. (Just (fileName destaque)), ProjectCover =. (Just (fileName cover))]
            redirect (PerfilProjectR projectid) 
        _ -> do
            setMessage [shamlet|
                <h1>
                    Problema em imagens
            |]
            redirect (CadProjImgsR projectid)
        
getPerfilProjectR :: ProjectId -> Handler Html
getPerfilProjectR projectId = do
    (logged, mUser, mProj) <- isLoggedAuthor projectId
    projeto <- runDB $ get404 projectId
    usuario <- runDB $ get404 $ projectCreator projeto
    userImg <- return $ StaticRoute ["img", "users", fromJust(userImg usuario)] []
    projetoDes <- return $ StaticRoute ["img", "proj", fromJust(projectDes projeto )] []
    projetoCover <- return $ StaticRoute ["img", "proj", fromJust(projectCover projeto)] []
    comentarios <- runDB $ selectList [CommentProject ==. projectId] [Desc CommentDateTime]
    comenuser <- sequence $ map (\ x -> (runDB $ get404 $ commentUser . entityVal $ x) >>= \y -> return (entityVal x,y)) comentarios
    (widget, enctype) <- generateFormPost $ formComment projectId 
    (wid, enc) <- generateFormPost formFinancing
    defaultLayout $ do
        setTitle "Talaka Pocket - Página de Campanha"
        $(whamletFile "templates/project.hamlet")
        
postApagarProjR :: ProjectId -> Handler Html
postApagarProjR projectId = do
    _ <- runDB $ get404 projectId
    ((result,_),_) <- runFormPost formApproved 
    case result of
        FormSuccess _ -> do
            runDB $ deleteWhere [CommentProject ==. projectId]
            runDB $ delete projectId
            redirect AdminR
        _ -> redirect AdminR
        


postFinanciarR :: ProjectId -> Handler Html
postFinanciarR projId = do
    _ <- runDB $ get404 projId
    (Just user) <- lookupSession "_USERID"
    Just (Entity userId _) <- runDB $ selectFirst [UserId ==. ( P.read . unpack $ user) ] []
    ((result,_),_) <- runFormPost formFinancing
    case result of
        FormSuccess ( vlFinancing ,typeFinancing) -> do
            actualDate <- fmap utctDay (lift $ liftIO getCurrentTime)    
            runDB $ insert (Financing userId projId vlFinancing actualDate typeFinancing)
            redirect (PerfilProjectR projId)
        _ -> do
            setMessage [shamlet|
                <h2> 
                    É necessário efetuar login
            |]
            redirect (PerfilProjectR projId)

