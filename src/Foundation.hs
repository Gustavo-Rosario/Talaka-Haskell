{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import Text.Cassius

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
    defaultLayout w = do
        p <- widgetToPageContent $ do
            w
            addStylesheet $ StaticR css_materialize_css
            addStylesheetRemote "https://fonts.googleapis.com/css?family=Courgette"
            addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_materialize_js
            toWidget $ $(cassiusFile "templates/css/main.cassius")
        msgs <- getMessages
        withUrlRenderer [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle p}
                    ^{pageHead p}
                <body>
                    $forall (status, msg) <- msgs
                        <p .message .#{status}>#{msg}>
                    ^{pageBody p}
        |]
    authRoute _ = Just LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized (PerfilUserR _) _ = return Authorized
    isAuthorized (PerfilProjectR _) _ = return Authorized
    isAuthorized CadUserR _ = return Authorized
    isAuthorized (CadUserImgsR _) _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isUser :: Handler AuthResult
isUser = do 
    session <- lookupSession "_USER"
    case session of 
        Nothing -> do
            admin <- lookupSession "_ADMIN"
            case admin of
                Nothing -> return AuthenticationRequired
                Just _ -> return Authorized
        Just _ -> return Authorized

isAdmin :: Handler AuthResult
isAdmin = do 
    session <- lookupSession "_ADMIN"
    case session of 
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized


-- instance YesodBreadcrumbs App where
--     breadcrumb HomeR = return ("Home", Nothing)
--     breadcrumb RootR = return ("Root", Just HomeR)
--     breadcrumb (NLR NLRootR) = return ("NL", Just RootR)
--     breadcrumb (UKR UKRootR) = return ("UK", Just RootR)
--     breadcrumb (BRR BRRootR) = return ("BR", Just RootR)

            

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
