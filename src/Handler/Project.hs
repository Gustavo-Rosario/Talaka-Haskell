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
import qualified Prelude as P

getCadProjR :: Handler Html
getCadProjR = undefined
-- do
--     session <- lookupSession "_USERID"
--     mUserId <- return $ fmap (P.read . unpack) session :: Handler (Maybe UserId)
--     userId <- fromJust(mUserId)
--     (widget, enctype) <- generateFormPost (formProject userId)
--     defaultLayout $ do
--         setTitle "Talaka Pocket - Cadastro Projeto"
--         $(whamletFile "templates/nav.hamlet")
--         $(whamletFile "templates/cadproj.hamlet")
        
postCadProjR :: Handler Html
postCadProjR = undefined 
-- do
--     session <- lookupSession "_USERID"
--     mUserId <- return $ fmap (P.read . unpack) session :: Handler (Maybe UserId)
--     userId <- fromJust(mUserId)
--     ((result,_),_) <- runFormPost (formProject userId) 
--     case result of
--         FormSuccess project -> do
--             projectid <- runDB $ insert project
--             redirect (CadProjImgsR projectid)
--         _ -> redirect HomeR 

getCadProjImgsR :: ProjectId -> Handler Html
getCadProjImgsR projectid = do 
    (widget, enctype) <- generateFormPost formProjectImg
    defaultLayout $ do
        setTitle "Talaka Pocket - Imagens do Projeto"
        $(whamletFile "templates/nav.hamlet")
        $(whamletFile "templates/projImg.hamlet")
        
postCadProjImgsR :: ProjectId -> Handler Html
postCadProjImgsR projectid = do
    _ <- runDB $ get404 projectid
    ((result,_),_) <- runFormPost formProjectImg
    case result of
        FormSuccess (destaque, cover) -> do
            liftIO $ fileMove destaque ("static/" ++ (unpack $ fileName destaque))
            liftIO $ fileMove cover ("static/" ++ (unpack $ fileName cover))
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
    projeto <- runDB $ get404 projectId
    usuario <- runDB $ get404 $ projectCreator projeto
    userImg <- return $ StaticRoute ["img", fromJust(userImg usuario)] []
    projetoDes <- return $ StaticRoute ["img", fromJust(projectDes projeto )] []
    projetoCover <- return $ StaticRoute ["img", fromJust(projectCover projeto)] []
    comentarios <- runDB $ selectList [CommentProject ==. projectId] [Desc CommentDateTime]
    comenuser <- sequence $ map (\ x -> (runDB $ get404 $ commentUser . entityVal $ x) >>= \y -> return (entityVal x,y)) comentarios
    (widget, enctype) <- generateFormPost $ formComment projectId 
    defaultLayout $ do
        setTitle "Talaka Pocket - Página de Campanha"
        $(whamletFile "templates/home.hamlet")
        $(whamletFile "templates/project.hamlet")
        
-- Listar Projetos        
getListProjR :: Handler Html
getListProjR = do
    projects <- runDB $ selectList [] [Desc ProjectDateBegin]
    projcreator <- sequence $ map (\proj -> (runDB $ get404 $ projectCreator . entityVal $ proj) >>= \creator -> return (entityVal proj, creator, entityKey proj) ) projects
    defaultLayout $ do
        setTitle "Talaka Pocket - Lista de Projetos"
        $(whamletFile "templates/listaprojetos.hamlet")
        $(whamletFile "templates/footer.hamlet")
        

postApagarProjR :: ProjectId -> Handler Html
postApagarProjR projectId = do
    _ <- runDB $ get404 projectId
    runDB $ delete projectId
    redirect ListProjR

selectProject :: Text -> Handler [Entity Project]
selectProject search = runDB $ rawSql "SELECT ?? FROM project WHERE title LIKE ?" [toPersistValue search]




-- getby
-- <-. maybe like