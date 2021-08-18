{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Perfil where

import Handler.Conta
import Handler.Transacao
import Import
import qualified Data.Text.Encoding   as TE (decodeUtf8)
import qualified Data.Text            as T (pack, unpack)

-- | Método GET que serve a página de Perfil, que serve como painel central para transferências
getPerfilR :: Int64 -> Handler Html
getPerfilR usuarioId = do
    usuarioLogadoId <- fromSqlKey . entityKey <$> requireAuth
    if usuarioLogadoId /= usuarioId 
        then do
            redirect HomeR
        else do
            req <- getRequest
            let mToken = reqToken req
            (novaContaForm, ncEnctype) <- generateFormPost formNovaConta
            usuario <- entityVal <$> requireAuth
            ettyContas <- runDB $ selectList [ContaUsuarioId ==. toSqlKey usuarioId] []
            ettyTransacoes <- runDB $ selectList [TransacaoEnviadoPor ==. toSqlKey usuarioId] []
            defaultLayout $ do
                setTitle . toHtml $ 
                    case usuarioNome usuario of
                        Just nome -> nome <> "'s User page"
                        Nothing -> ""
                $(widgetFile "usuario/perfil")
