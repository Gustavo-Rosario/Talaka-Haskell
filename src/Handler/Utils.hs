{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Utils where

import Import
import Data.Maybe
import Data.Time
import qualified Prelude as P


dateFormat :: String -> String
dateFormat (y1:y2:y3:y4:_:m1:m2:_:d1:d2:[]) = [d1,d2,'/',m1,m2,'/',y1,y2,y3,y4]