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
import qualified Prelude as P


postComentarProjectR :: ProjectId -> Handler Html
postComentarProjectR pId = do
    session <- lookupSession "_USERID"
    case session of
        Just user -> do
            Just (Entity userId _) <- runDB $ selectFirst [UserId ==. ( P.read . unpack $ user) ] []
            ((result,_),_) <- runFormPost $ formComment pId
            case result of
                FormSuccess (Comment cmt _ proj dt r) -> do
                    runDB $ insert (Comment cmt userId proj dt r) :: Handler CommentId
                    redirect $ PerfilProjectR pId
                _ -> redirect HomeR
        _ -> do
            setMessage [shamlet|
                <h2>Voce precisa estar logado para comentar</h2>
            |]
            redirect $ PerfilProjectR pId