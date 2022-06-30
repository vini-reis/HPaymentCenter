{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Account where

import Handler.Common
import Import hiding (Active, Inactive)

-- | Neste handler foi utilizada uma outra abordagem para criar e capturas
-- | formulários, que, utiliza mais do framework, mas requer mais código
-- | para formatar o front-end

-- | Tipos de conta
data AccountType = Personal | Business
    deriving (Show, Read, Eq)

-- | Tipos de status para as contas
data AccountStatus = Active | Inactive
    deriving (Show, Read, Eq)

accountTypes :: [AccountType]
accountTypes = [Personal, Business]

accountStatuses :: [AccountStatus]
accountStatuses = [Active, Inactive]

-- | Tipo de dado utilizado para receber novas contas
data NewAccount = NewAccount
    { newAccountUserId :: Int64
    , newAccountName   :: Text
    , newAccountType   :: AccountType
    , newAccountStatus  :: AccountStatus
    }

-- | Criando formulário através do framework
formNewAccount :: Form NewAccount
formNewAccount = renderBootstrap4 BootstrapBasicForm $ NewAccount 
    <$> areq hiddenField configTextUserId Nothing
    <*> areq textField configTextName (Just "Personal Account")
    <*> areq selectTipo configSelectType (Just Personal)
    <*> areq selectStatus configSelectStatus (Just Active)
    where
        selectTipo = selectFieldList [(pack $ show Personal :: Text, Personal), (pack $ show Business :: Text, Business)]
        selectStatus = selectFieldList [("Active" :: Text, Active), ("Inactive" :: Text, Inactive)]
        configTextUserId = 
            FieldSettings 
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Just "userId"
                , fsName = Just "userId"
                , fsAttrs = []
                }
        configTextName = 
            FieldSettings 
                { fsLabel = "Name"
                , fsTooltip = Nothing
                , fsId = Just "name"
                , fsName = Just "name"
                , fsAttrs = [("class", "form-control"), ("placeholder", "Name")]
                }
        configSelectType = 
            FieldSettings 
                { fsLabel = "Type"
                , fsTooltip = Nothing
                , fsId = Just "type"
                , fsName = Just "type"
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
postSaveAccountR :: Handler Value
postSaveAccountR = do
    ((result, _), _) <- runFormPost formNewAccount
    case result of
        FormSuccess newAccount -> do
            ettyConta <- runDB $ insertEntity addedAccount
            let response = Return 
                    { success = True
                    , message = "Account saved successfully"
                    , returnData = entityVal ettyConta
                    }
            returnJson response
            where
                addedAccount = Account
                    { accountUserId  = toSqlKey $ newAccountUserId newAccount
                    , accountName    = newAccountName newAccount
                    , accountType    = fromString . show $ newAccountType newAccount
                    , accountStatus  = fromString . show $ newAccountStatus newAccount
                    , accountBalance = 0
                    }
        _ -> returnJson Return 
                { success = False
                , message = "Falha"
                , returnData = ()
                }

-- Método POST que recebe as req. para atualizar dados de uma conta existente
postUpdateAccountR :: Text -> Handler Value
postUpdateAccountR accountId = do
    let accountKey = AccountKey { unAccountKey = accountId }
    mAccount <- runDB $ get accountKey
    case mAccount of
        Nothing -> returnJson Return 
                { success = False
                , message = "Account not found"
                , returnData = ()
                }
        Just account -> do
            mBalance <- lookupPostParam "accountBalance"
            mType <- lookupPostParam "accountType"
            mStatus <- lookupPostParam "accountStatus"
            let tipe = fromMaybe "" mType
            let status = fromMaybe "" mStatus
            case mBalance of
                Nothing -> returnJson Return 
                    { success = False
                    , message = "Invalid balance value"
                    , returnData = ()
                    }
                Just saldo -> do
                    case double saldo of
                        Left errorMsg -> do
                            returnJson Return 
                                { success = False
                                , message = pack errorMsg
                                , returnData = ()
                                }
                        Right (dBalance, _) -> do
                            currentDate <- liftIO $ pack . showGregorian . utctDay <$> getCurrentTime
                            let transaction = Transaction
                                    { transactionSentBy     = accountUserId account
                                    , transactionSentFrom   = accountKey
                                    , transactionReceivedBy = accountKey
                                    , transactionSentIn     = currentDate
                                    , transactionValue      = dBalance - accountBalance account
                                    }
                            _ <- runDB $ insert transaction
                            runDB $ update accountKey [AccountBalance =. dBalance, AccountStatus =. status, AccountType =. tipe]
                            returnJson Return 
                                { success = True
                                , message = "Account updated"
                                , returnData = account
                                }

-- | Método GET para pesquisar contas pelo e-mail para facilitar transferências
getSearchAccountR :: Text -> Handler Value
getSearchAccountR email = do
    mUser <- runDB $ getBy $ UniqueUser email
    case mUser of
        Nothing -> returnJson 
            Return 
                { success = False
                , message = "Account not found"
                , returnData = ()
                }
        Just user -> do
            account <- runDB $ selectFirst [AccountUserId ==. entityKey user] []
            returnJson
                Return 
                    { success = True
                    , message = "Account found successfully"
                    , returnData = account
                    }

-- | Método que funciona como um join para a entidade de conta.
-- | Quer dizer, receve uma key de um tipo conta, seleciona no banco
-- | e retorna a propriedade que queremos, neste caso, o nome e seu Id
-- | numa elemento de célula para uma tabela em HTML
showAccountName :: Key Account -> Widget
showAccountName key = do
    mAccount <- handlerToWidget $ runDB $ get key
    [whamlet|
        $maybe account <- mAccount
            <td>
                <span data-bs-toggle="tooltip" data-bs-placement="top" title="ID: #{unAccountKey key}">#{accountName account}
    |]