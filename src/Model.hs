{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Sql (PersistFieldSql, sqlType)
import Database.Persist.Quasi (lowerCaseSettings)
import Data.UUID               as UUID
import Data.ByteString.Char8   as B8
import Yesod.Auth.HashDB
import GHC.Generics

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

instance PersistField UUID where
    toPersistValue u = PersistDbSpecific . B8.pack . UUID.toString $ u
    fromPersistValue (PersistDbSpecific t) =
        case UUID.fromString $ B8.unpack t of
        Just x -> Right x
        Nothing -> Left "Invalid UUID"
    fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
    sqlType _ = SqlOther "uuid"

instance HashDBUser Usuario where
    userPasswordHash = usuarioSenha

    setPasswordHash senha usuario = usuario { usuarioSenha = Just senha }
