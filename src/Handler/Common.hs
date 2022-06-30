{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DatatypeContexts #-}

-- | Common handler functions.
module Handler.Common where

import Import

-- | ADT que serve como objeto JSON de retorno das requisições
data (ToJSON a) => Return a = Return
    { success    :: Bool
    , message    :: Text
    , returnData           :: a
    } deriving (Show, Read)

-- | Instância de ToJSON para Return de um tipo a que possa ser convertido para JSON
instance (ToJSON a) => ToJSON (Return a) where
    toJSON (Return s m o) = object ["success" .= s, "message" .= m, "returnData" .= toJSON o]

-- | Instância de JSON para entidades do banco de dados
-- | utilizada para coletar os ids das entidades no front-end
instance (PersistEntity a, ToJSON a) => ToJSON (Entity a) where
    toJSON = keyValueEntityToJSON

-- | Instâncias de ToJSON para as entidades criadas
instance ToJSON Account where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Transaction where
    toEncoding = genericToEncoding defaultOptions