{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Text.Cassius
import Database.Persist.Postgresql
import Handler.Form
import Data.Maybe
import Handler.Utils

getAdminR :: Handler Html
getAdminR = do 
    projects <- runDB $ selectList [ProjectApproved ==. 0] [Desc ProjectDateBegin]
    projcreator <- sequence $ map (\proj -> (runDB $ get404 $ projectCreator . entityVal $ proj) >>= \creator -> return (entityVal proj, creator, entityKey proj) ) projects
    (widget, enctype) <- generateFormPost formApproved
    defaultLayout $ do
        setTitle "Talaka Pocket - Dashboard"
        -- $(whamletFile "templates/home.hamlet")
        $(whamletFile "templates/dash.hamlet")

postProjectAprrovedR :: ProjectId -> Handler Html
postProjectAprrovedR projId = do 
    _ <- runDB $ get404 projId
    ((result, _), _) <- runFormPost formApproved
    case result of
        FormSuccess (admin, talaka) -> do
            runDB $ update projId [ProjectApproved =. 1]
            redirect AdminR
        _ -> do
            setMessage [shamlet|
                <h1> Not Allowed </h1>
            |]
            redirect HomeR
