{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Comment where

import Import
import Text.Cassius
import Database.Persist.Postgresql
import Handler.Form


postComentarProjectR :: ProjectId -> Handler Html
postComentarProjectR pId = do
    ((result,_),_) <- runFormPost $ formComment pId
    case result of
        FormSuccess comment -> do
            runDB $ insert comment :: Handler CommentId
            redirect $ PerfilProjectR pId
        _ -> redirect HomeR