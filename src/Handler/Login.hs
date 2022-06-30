{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import

data LoginForm = LoginForm 
    { userUsername :: Text
    , userPassword :: Text
    }

getLoginFormR :: Handler Html
getLoginFormR = do
    defaultLayout $ do
        setTitle "Login"
        $(widgetFile "user/login")
