{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Conta where

import Handler.Common
import Import hiding (Active, Inactive)
import qualified Data.Text              as T (pack, unpack)
import qualified Data.Text.Encoding     as TE (encodeUtf8, decodeUtf8)

-- | Neste handler foi utilizada uma outra abordagem para criar e capturas
-- | formulários, que, utiliza mais do framework, mas requer mais código
-- | para formatar o front-end

-- | Tipo de dado utilizado para receber novas contas
data NovaConta = NovaConta
    { ncUsuarioId   :: Int64
    , ncNome        :: Text
    , ncTipo        :: ContaTipo
    , ncStatus      :: ContaStatus
    }

-- | Tipos de conta
data ContaTipo = Personal | Business
    deriving (Show, Read, Eq)

tiposConta :: [ContaTipo]
tiposConta = [Personal, Business]

-- | Tipos de status para as contas
data ContaStatus = Active | Inactive
    deriving (Show, Read, Eq)

statusConta :: [ContaStatus]
statusConta = [Active, Inactive]

-- | Criando formulário através do framework
formNovaConta :: Form NovaConta
formNovaConta = renderBootstrap4 BootstrapBasicForm $ NovaConta 
    <$> areq hiddenField configCampoUsuarioId Nothing
    <*> areq textField configCampoNome (Just "Personal Account")
    <*> areq selectTipo configSelectTipo (Just Personal)
    <*> areq selectStatus configSelectStatus (Just Active)
    where
        selectTipo = selectFieldList [(T.pack $ show Personal, Personal), (T.pack $ show Business, Business)]
        selectStatus = selectFieldList [("Active" :: Text, Active), ("Inactive" :: Text, Inactive)]
        configCampoUsuarioId = 
            FieldSettings 
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Just "usuarioId"
                , fsName = Just "usuarioId"
                , fsAttrs = []
                }
        configCampoNome = 
            FieldSettings 
                { fsLabel = "Name"
                , fsTooltip = Nothing
                , fsId = Just "nome"
                , fsName = Just "nome"
                , fsAttrs = [("class", "form-control"), ("placeholder", "Name")]
                }
        configSelectTipo = 
            FieldSettings 
                { fsLabel = "Type"
                , fsTooltip = Nothing
                , fsId = Just "tipo"
                , fsName = Just "tipo"
                , fsAttrs = [("class", "form-select")]
                }
        configSelectStatus = 
            FieldSettings 
                { fsLabel = "Status"
                , fsTooltip = Nothing
                , fsId = Just "status"
                , fsName = Just "status"
                , fsAttrs = [("class", "form-select")]
                }

-- | Método POST que recebe a requisição para salvar uma nova conta
postSalvarContaR :: Handler Value
postSalvarContaR = do
    ((result, _), _) <- runFormPost formNovaConta
    case result of
        FormSuccess novaConta -> do
            ettyConta <- runDB $ insertEntity contaAdicionada
            let resposta = Retorno 
                    { sucesso = True
                    , mensagem = "Account saved successfully"
                    , objeto = entityVal ettyConta
                    }
            returnJson resposta
            where
                contaAdicionada = Conta
                    { contaUsuarioId    = toSqlKey $ ncUsuarioId novaConta
                    , contaNome         = ncNome novaConta
                    , contaTipo         = fromString . show $ ncTipo novaConta
                    , contaStatus       = fromString . show $ ncStatus novaConta
                    , contaSaldo        = 0
                    }
        _ -> returnJson Retorno 
                { sucesso = False
                , mensagem = "Falha"
                , objeto = ()
                }

-- Método POST que recebe as req. para atualizar dados de uma conta existente
postAtualizarContaR :: Text -> Handler Value
postAtualizarContaR contaId = do
    let contaKey = ContaKey { unContaKey = contaId }
    mConta <- runDB $ get contaKey
    case mConta of
        Nothing -> returnJson Retorno 
                { sucesso = False
                , mensagem = "Account not found"
                , objeto = ()
                }
        Just conta -> do
            mSaldo <- lookupPostParam "contaSaldo"
            mTipo <- lookupPostParam "contaTipo"
            mStatus <- lookupPostParam "contaStatus"
            let tipo = fromMaybe "" mTipo
            let status = fromMaybe "" mStatus
            case mSaldo of
                Nothing -> returnJson Retorno 
                    { sucesso = False
                    , mensagem = "Invalid balance value"
                    , objeto = ()
                    }
                Just saldo -> do
                    case double saldo of
                        Left errorMsg -> do
                            returnJson Retorno 
                                { sucesso = False
                                , mensagem = T.pack errorMsg
                                , objeto = ()
                                }
                        Right (dSaldo, _) -> do
                            dataAtual <- liftIO $ T.pack . showGregorian . utctDay <$> getCurrentTime
                            let transacao = Transacao
                                    { transacaoEnviadoPor    = contaUsuarioId conta
                                    , transacaoEnviadoDe     = contaKey
                                    , transacaoRecebidoPor   = contaKey
                                    , transacaoEnviadoEm     = dataAtual
                                    , transacaoValor         = dSaldo - contaSaldo conta
                                    }
                            runDB $ insert transacao
                            runDB $ update contaKey [ContaSaldo =. dSaldo, ContaStatus =. status, ContaTipo =. tipo]
                            returnJson Retorno 
                                { sucesso = True
                                , mensagem = "Account updated"
                                , objeto = conta
                                }

-- | Método GET para pesquisar contas pelo e-mail para facilitar transferências
getPesquisarContaR :: Text -> Handler Value
getPesquisarContaR email = do
    mUsuarioEtty <- runDB $ getBy $ UniqueUser email
    case mUsuarioEtty of
        Nothing -> returnJson 
            Retorno 
                { sucesso = False
                , mensagem = "Account not found"
                , objeto = ()
                }
        Just usuarioEtty -> do
            ettyConta <- runDB $ selectFirst [ContaUsuarioId ==. entityKey usuarioEtty] []
            returnJson
                Retorno 
                    { sucesso = True
                    , mensagem = "Account found successfully"
                    , objeto = ettyConta
                    }
-- | Método que funciona como um join para a entidade de conta.
-- | Quer dizer, receve uma key de um tipo conta, seleciona no banco
-- | e retorna a propriedade que queremos, neste caso, o nome e seu Id
-- | numa elemento de célula para uma tabela em HTML
showContaNome :: Key Conta -> Widget
showContaNome key = do
    mConta <- handlerToWidget $ runDB $ get key
    [whamlet|
        $maybe conta <- mConta
            <td>
                <span data-bs-toggle="tooltip" data-bs-placement="top" title="ID: #{unContaKey key}">#{contaNome conta}
    |]