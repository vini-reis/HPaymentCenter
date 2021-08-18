{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common handler functions.
module Handler.Common where

import Import

-- | ADT que serve como objeto JSON de retorno das requisições
data (ToJSON a) => Retorno a = Retorno
    { sucesso :: Bool
    , mensagem :: Text
    , objeto :: a
    } deriving (Show, Read)

-- | Instância de ToJSON para Retorno de um tipo a que possa ser convertido para JSON
instance (ToJSON a) => ToJSON (Retorno a) where
    toJSON (Retorno s m o) = object ["sucesso" .= s, "mensagem" .= m, "objeto" .= toJSON o]

-- | Instância de JSON para entidades do banco de dados
-- | utilizada para coletar os ids das entidades no front-end
instance (PersistEntity a, ToJSON a) => ToJSON (Entity a) where
    toJSON = keyValueEntityToJSON

-- | Instâncias de ToJSON para as entidades criadas
instance ToJSON Conta where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Transacao where
    toEncoding = genericToEncoding defaultOptions