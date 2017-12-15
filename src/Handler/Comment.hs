{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Comment where

import Import
import Text.Cassius()
import Database.Persist.Postgresql(toSqlKey, fromSqlKey)
import Handler.Form
import Handler.Utils
import Data.Maybe(fromJust)
import Data.Aeson (toJSON, fromJSON)
import qualified Prelude as P


postComentarProjectR :: ProjectId -> Handler Value
postComentarProjectR pId = do
    session <- lookupSession "_USERID"
    case session of
        Just user -> do
            userId <- return $ ( toSqlKey . P.read . unpack $ user)
            ((result,_),_) <- runFormPost $ formComment pId
            case result of
                FormSuccess (Comment cmt _ proj dt r) -> do
                    _ <- runDB $ insert (Comment cmt userId proj dt r) :: Handler CommentId
                    (Just u) <- lookupSession "_USER"
                    userInfo <- return $ (P.read . unpack $ u)  :: Handler User
                    sendResponseStatus status201 $ toJSON (JSONResponse "success" (userName userInfo))
                _ -> sendResponseStatus status203 $ toJSON (JSONResponse "fail" "Not Allow")
        _ -> sendResponseStatus status200 $ toJSON (JSONResponse "fail" "Not Allow")

-- getCommentAjaxR :: Handler Value
-- getCommentAjaxR = do
    