{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Transaction where

import Handler.Common
import Import

-- | Método POST que processa todas as transações e guarda seus registros 
-- | na tabela Transacao no banco de dados. O método processa, valida
-- | e guarda a transação somente se esta estiver OK. 
postTransactionR :: Handler Value
postTransactionR = do
    mUserId <- lookupPostParam "sentBy"
    mSenderAccountId <- lookupPostParam "sentFrom"
    mReceiverAccountId <- lookupPostParam "receivedBy"
    mValue <- lookupPostParam "value"
    let userId = fromMaybe "" mUserId
        intUserId = case decimal userId of
                        Left _ -> 0
                        Right (value, _) -> value
        senderAccountKey = AccountKey { unAccountKey = fromMaybe "" mSenderAccountId }
        receiverAccountKey = AccountKey { unAccountKey = fromMaybe "" mReceiverAccountId }
        tValue = fromMaybe "" mValue
    
    putStrLn $ pack "Valor recebido: " ++ pack (show mValue)

    case double tValue of
        Left errorMsg -> returnJson Return { success = False, message = "Invalid value", returnData = () }
        Right (value, _) -> do
            mSenderAccount <- runDB $ get senderAccountKey
            case mSenderAccount of
                Nothing -> returnJson Return { success = False, message = "Sender's account not found", returnData = () }
                Just senderAccount -> do
                    mReceiverAccount <- runDB $ get receiverAccountKey
                    case mReceiverAccount of
                        Nothing -> returnJson Return { success = False, message = "Receiver's account not found", returnData = () }
                        Just receiverAccount -> do
                            let senderBalance = accountBalance senderAccount - value
                            let receiverBalance = accountBalance receiverAccount + value
                            if senderBalance < 0 
                            then 
                                returnJson Return { success = False, message = "Insufficient funds", returnData = () }
                            else
                                do
                                    currentDate <- liftIO $ pack . showGregorian . utctDay <$> getCurrentTime
                                    runDB $ update senderAccountKey [AccountBalance =. senderBalance]
                                    runDB $ update receiverAccountKey [AccountBalance =. receiverBalance]
                                    ettyTransaction <- runDB $ insertEntity Transaction
                                        { transactionSentBy     = toSqlKey intUserId
                                        , transactionSentFrom   = senderAccountKey
                                        , transactionReceivedBy = receiverAccountKey
                                        , transactionSentIn     = currentDate
                                        , transactionValue      = value
                                        }
                                    returnJson Return
                                        { success = True
                                        , message = "Transaction completed successfully"
                                        , returnData = entityVal ettyTransaction
                                        }
