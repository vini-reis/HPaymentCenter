{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Perfil where

import Handler.Account
import Handler.Transaction
import Import
import qualified Data.Text.Encoding   as TE (decodeUtf8)
import qualified Data.Text            as T (pack, unpack)

-- | Método GET que serve a página de Perfil, que serve como painel central para transferências
getPerfilR :: Int64 -> Handler Html
getPerfilR userId = do
    loggedUserId <- fromSqlKey . entityKey <$> requireAuth
    if loggedUserId /= userId 
        then do
            redirect HomeR
        else do
            req <- getRequest
            let mToken = reqToken req
            (formNewAccount, ncEnctype) <- generateFormPost formNewAccount
            user <- entityVal <$> requireAuth
            ettyAccounts <- runDB $ selectList [AccountUserId ==. toSqlKey userId] []
            ettyTransactions <- runDB $ selectList [TransactionSentBy ==. toSqlKey userId] []
            defaultLayout $ do
                setTitle . toHtml $ 
                    case userName user of
                        Just name -> name <> "'s User page"
                        Nothing -> ""
                $(widgetFile "user/profile")
