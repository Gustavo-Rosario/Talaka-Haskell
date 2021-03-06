{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Utils where

import Import
import Data.Maybe
import Data.Time()
import Database.Persist.Postgresql(toSqlKey, fromSqlKey)
import qualified Prelude as P
import Data.Aeson
import GHC.Generics

data JSONResponse = JSONResponse { stats :: Text, msg :: Text } deriving (Show, Generic)

instance FromJSON JSONResponse where
    parseJSON (Object v) =
        JSONResponse <$> v .: "stats"
                     <*> v .: "msg"
    parseJSON _ = mzero

instance ToJSON JSONResponse where
    toJSON (JSONResponse stats msg) = object [ "stats"  .= stats, "msg"   .= msg]

dateFormat :: String -> String
dateFormat (y1:y2:y3:y4:_:m1:m2:_:d1:d2:[]) = [d1,d2,'/',m1,m2,'/',y1,y2,y3,y4]

dateFormatMonth :: String -> String
dateFormatMonth (y1:y2:y3:y4:_:'0':'1':_:d1:d2:[]) = [d1,d2]++" de Janeiro de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'2':_:d1:d2:[]) = [d1,d2]++" de Fevereiro de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'3':_:d1:d2:[]) = [d1,d2]++" de Março de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'4':_:d1:d2:[]) = [d1,d2]++" de Abril de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'5':_:d1:d2:[]) = [d1,d2]++" de Maio de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'6':_:d1:d2:[]) = [d1,d2]++" de Junho de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'7':_:d1:d2:[]) = [d1,d2]++" de Julho de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'8':_:d1:d2:[]) = [d1,d2]++" de Agosto de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'0':'9':_:d1:d2:[]) = [d1,d2]++" de Setembro de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'1':'0':_:d1:d2:[]) = [d1,d2]++" de Outubro de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'1':'1':_:d1:d2:[]) = [d1,d2]++" de Novembro de "++[y1,y2,y3,y4]
dateFormatMonth (y1:y2:y3:y4:_:'1':'2':_:d1:d2:[]) = [d1,d2]++" de Dezembro de "++[y1,y2,y3,y4]

getImgStatic :: [Text] -> Text -> Route Static
getImgStatic path fileName = StaticRoute (path++[fileName]) []

isLogged :: Handler (Maybe Int, Maybe UserId)
isLogged = do 
    session <- lookupSession "_USERID"
    case session of 
        Nothing -> do
            admin <- lookupSession "_ADMIN"
            case admin of
                Nothing -> return (Nothing,Nothing)
                Just _ -> return (Just 2, Nothing)
        Just _ -> do
            Just(Entity userId user) <- runDB $ selectFirst [UserId ==. (toSqlKey . P.read . unpack $ fromJust(session)) ] []
            return (Just 1, Just userId)

calcPercent :: Int -> Int -> Float
calcPercent x y =  100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

isAuthor :: ProjectId -> UserId -> Handler (Maybe Int)
isAuthor pid uid = do
    mProj <- runDB $ selectFirst [ProjectId ==. pid, ProjectCreator ==. uid] []
    case mProj of
        Just _ -> do
            return (Just 1)
        Nothing -> return Nothing

isLoggedAuthor :: ProjectId -> Handler (Maybe Int, Maybe UserId, Maybe (Entity Project))
isLoggedAuthor pid = do 
    session <- lookupSession "_USERID"
    case session of 
        Nothing -> do
            admin <- lookupSession "_ADMIN"
            case admin of
                Nothing -> return (Nothing,Nothing,Nothing)
                Just _ -> return (Just 2, Nothing,Nothing)
        Just _ -> do
            Just(Entity userId user) <- runDB $ selectFirst [UserId ==. (  toSqlKey . P.read . unpack $ fromJust(session)) ] []
            mProj <- runDB $ selectFirst [ProjectId ==. pid, ProjectCreator ==. userId] []
            return (Just 1, Just userId, mProj)

    