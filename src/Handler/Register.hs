{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Register where

import Import

data NewUser = NewUser
    { newUserName        :: Text
    , newUserEmail       :: Text
    , newUserPassword    :: Text
    } deriving Show

-- | Formulário usado para fazer login
formNewUser :: Form NewUser
formNewUser = renderBootstrap4 BootstrapBasicForm $ NewUser 
    <$> areq textField configTextName Nothing
    <*> areq emailField configTextEmail Nothing
    <*> areq textField configTextPassword Nothing
    where
        configTextName = 
            FieldSettings 
                { fsLabel = "Name"
                , fsTooltip = Nothing
                , fsId = Just "name"
                , fsName = Just "name"
                , fsAttrs = [("class", "form-control"), ("placeholder", "Name")]
                }
        configTextEmail = 
            FieldSettings 
                { fsLabel = "E-mail"
                , fsTooltip = Nothing
                , fsId = Just "email"
                , fsName = Just "email"
                , fsAttrs = [("class", "form-control"), ("placeholder", "E-mail")]
                }
        configTextPassword = 
            FieldSettings 
                { fsLabel = "Password"
                , fsTooltip = Nothing
                , fsId = Just "password"
                , fsName = Just "password"
                , fsAttrs = [("class", "form-control"), ("placeholder", "Password")]
                }

-- | Método GET que serve o widget para cadastro de novos usuários
getRegisterR :: Handler Html
getRegisterR = do
    (nuLoginForm, nuEnctype) <- generateFormPost formNewUser
    defaultLayout $ do
        setTitle "Register"
        $(widgetFile "user/register")

-- | Método POST que recebe a requisição para cadastrar um novo usuário
postRegisterR :: Handler Html
postRegisterR = do
    ((result, _), _) <- runFormPost formNewUser
    case result of
        FormSuccess newUser -> do
            user <- setPassword (newUserPassword newUser) addedUser
            userId <- fromSqlKey <$> addVerifiedUser user
            redirect HomeR
            where
                addedUser = User
                    { userName     = Just . newUserName $ newUser
                    , userEmail    = newUserEmail newUser
                    , userPassword = Nothing
                    , userKey      = Nothing
                    , userVerified = True
                    }

-- | Handler que adiciona um usuário no banco de dados
addVerifiedUser :: User -> HandlerFor App (Key User)
addVerifiedUser user = runDB $ insert user
