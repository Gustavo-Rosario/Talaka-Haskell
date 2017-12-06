{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Search where

import Import
import Text.Cassius
import Database.Persist.Postgresql
import Handler.Form
import Data.Maybe
import Handler.Utils

getExploreR :: Handler Html
getExploreR = do 
    logged <- isLogged
    projects <- runDB $ selectList [] [Desc ProjectDateBegin]
    projcreator <- sequence $ map (\proj -> (runDB $ get404 $ projectCreator . entityVal $ proj) >>= \creator -> return (entityVal proj, creator, entityKey proj) ) projects
    (widget, enctype) <- generateFormPost formSearch
    defaultLayout $ do
        setTitle "Talaka Pocket - Explore Projetos"
        $(whamletFile "templates/nav.hamlet")
        $(whamletFile "templates/listaprojetos.hamlet")
        $(whamletFile "templates/footer.hamlet")

postBuscaAuxR :: Handler Html
postBuscaAuxR = do
    ((result,_),_) <- runFormPost formSearch
    case result of
        FormSuccess (search, _ ) -> do
            redirect (BuscaR search)
        _ -> do
            setMessage [shamlet|
                <h1>
                    Erro no Formulario
            |]
            redirect ExploreR


getBuscaR :: Text -> Handler Html
getBuscaR search = do
    logged <- isLogged
    projects <- selectProject ("%" <> search <> "%")
    projcreator <- sequence $ map (\proj -> (runDB $ get404 $ projectCreator . entityVal $ proj) >>= \creator -> return (entityVal proj, creator, entityKey proj) ) projects
    (widget, enctype) <- generateFormPost formSearch
    defaultLayout $ do
        setTitle "Talaka Pocket - Explore Projetos"
        $(whamletFile "templates/nav.hamlet")
        $(whamletFile "templates/listaprojetos.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
selectProject :: Text -> Handler [Entity Project]
selectProject search = runDB $ rawSql "SELECT ?? FROM project WHERE title LIKE ? ORDER BY date_begin DESC" [toPersistValue search]
