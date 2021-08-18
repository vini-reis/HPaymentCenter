{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Transacao where

import Handler.Common
import Import
import qualified Data.Text            as T (pack, unpack)

-- | Método POST que processa todas as transações e guarda seus registros 
-- | na tabela Transacao no banco de dados. O método processa, valida
-- | e guarda a transação somente se esta estiver OK. 
postTransacaoR :: Handler Value
postTransacaoR = do
    mUsuarioId <- lookupPostParam "enviadoPor"
    mContaOrigemId <- lookupPostParam "enviadoDe"
    mContaDestinoId <- lookupPostParam "recebidoPor"
    mValor <- lookupPostParam "valor"
    let usuarioId = fromMaybe "" mUsuarioId
        intUsuarioId = case decimal usuarioId of
                        Left _ -> 0
                        Right (valor, _) -> valor
        contaOrigemKey = ContaKey { unContaKey = fromMaybe "" mContaOrigemId }
        contaDestinoKey = ContaKey { unContaKey = fromMaybe "" mContaDestinoId }
        tValor = fromMaybe "" mValor
    
    putStrLn $ T.pack "Valor recebido: " ++ T.pack (show mValor)

    case double tValor of
        Left errorMsg -> returnJson Retorno { sucesso = False, mensagem = "Invalid value", objeto = () }
        Right (valor, _) -> do
            mContaOrigem <- runDB $ get contaOrigemKey
            case mContaOrigem of
                Nothing -> returnJson Retorno { sucesso = False, mensagem = "Origin account not found", objeto = () }
                Just contaOrigem -> do
                    mContaDestino <- runDB $ get contaDestinoKey
                    case mContaDestino of
                        Nothing -> returnJson Retorno { sucesso = False, mensagem = "Destiny account not found", objeto = () }
                        Just contaDestino -> do
                            let saldoOrigem = contaSaldo contaOrigem - valor
                            let saldoDestino = contaSaldo contaDestino + valor
                            if saldoOrigem < 0 
                            then 
                                returnJson Retorno { sucesso = False, mensagem = "Insufficient funds", objeto = () }
                            else
                                do
                                    dataAtual <- liftIO $ T.pack . showGregorian . utctDay <$> getCurrentTime
                                    runDB $ update contaOrigemKey [ContaSaldo =. saldoOrigem]
                                    runDB $ update contaDestinoKey [ContaSaldo =. saldoDestino]
                                    ettyTransacao <- runDB $ insertEntity Transacao 
                                        { transacaoEnviadoPor    = toSqlKey intUsuarioId
                                        , transacaoEnviadoDe     = contaOrigemKey
                                        , transacaoRecebidoPor   = contaDestinoKey
                                        , transacaoEnviadoEm     = dataAtual
                                        , transacaoValor         = valor
                                        }
                                    returnJson Retorno
                                        { sucesso = True
                                        , mensagem = "Transaction completed successfully"
                                        , objeto = entityVal ettyTransacao
                                        }
