{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Cadastrar where

import Handler.Common
import Import

data NovoUsuario = NovoUsuario
    { nuNome        :: Text
    , nuEmail       :: Text
    , nuSenha    :: Text
    } deriving Show

-- | Formulário usado para fazer login
formNovoUsuario :: Form NovoUsuario
formNovoUsuario = renderBootstrap4 BootstrapBasicForm $ NovoUsuario 
    <$> areq textField configCampoNome Nothing
    <*> areq emailField configCampoEmail Nothing
    <*> areq textField configCampoSenha Nothing
    where
        configCampoNome = 
            FieldSettings 
                { fsLabel = "Name"
                , fsTooltip = Nothing
                , fsId = Just "name"
                , fsName = Just "name"
                , fsAttrs = [("class", "form-control"), ("placeholder", "Name")]
                }
        configCampoEmail = 
            FieldSettings 
                { fsLabel = "E-mail"
                , fsTooltip = Nothing
                , fsId = Just "email"
                , fsName = Just "email"
                , fsAttrs = [("class", "form-control"), ("placeholder", "E-mail")]
                }
        configCampoSenha = 
            FieldSettings 
                { fsLabel = "Password"
                , fsTooltip = Nothing
                , fsId = Just "password"
                , fsName = Just "password"
                , fsAttrs = [("class", "form-control"), ("placeholder", "Password")]
                }

-- | Método GET que serve o widget para cadastro de novos usuários
getCadastrarR :: Handler Html
getCadastrarR = do
    (nuLoginForm, nuEnctype) <- generateFormPost formNovoUsuario
    defaultLayout $ do
        setTitle "Register"
        $(widgetFile "usuario/cadastrar")

-- | Método POST que recebe a requisição para cadastrar um novo usuário
postCadastrarR :: Handler Html
postCadastrarR = do
    ((result, _), _) <- runFormPost formNovoUsuario
    case result of
        FormSuccess novoUsuario -> do
            usuario <- setPassword (nuSenha novoUsuario) usuarioAdicionado
            usuarioId <- fromSqlKey <$> addVerifiedUser usuario
            redirect HomeR
            where
                usuarioAdicionado = Usuario
                    { usuarioNome       = Just . nuNome $ novoUsuario
                    , usuarioEmail      = nuEmail novoUsuario
                    , usuarioSenha      = Nothing
                    , usuarioChave      = Nothing
                    , usuarioVerificado = True
                    }

-- | Handler que adiciona um usuário no banco de dados
addVerifiedUser :: Usuario -> HandlerFor App (Key Usuario)
addVerifiedUser usuario = runDB $ insert usuario
