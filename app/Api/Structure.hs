{-# LANGUAGE QuasiQuotes #-}

module Api.Structure where

import Data.Text
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import qualified Hasql.Session as H
import qualified Hasql.Statement as H
import qualified Hasql.Connection as H
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Connection as H.Connection
import qualified Hasql.Session as H.Session
import qualified Hasql.TH as TH

import Data.Vector (Vector)
import Control.Monad.Reader
import Contravariant.Extras.Contrazip (contrazip2)
import Data.String.Conversions (cs)

import Api.Query (asJsonRow, whereT)
import GHC.Int (Int64)
import qualified Network.HTTP.Simple as Net

type PGQuery a = Reader H.Connection a

data User = User { 
  login :: Text
, name :: Maybe Text
, password :: Maybe Text 
} deriving Show

instance JSON.ToJSON User where
  toJSON v = JSON.object [
      "login" .= login v
    , "name"  .= name v
    , "password" .= password v ]

data ForeignKey = ForeignKey {
  fkTable :: Text
, fkCol :: Text
} deriving (Eq, Show)

data Table = Table {
  tableSchema :: Text
, tableName :: Text
, tableInsertable :: Bool
} deriving (Show)

instance JSON.ToJSON Table where
  toJSON v = JSON.object [
      "table_schema" .= tableSchema v
    , "table_name" .= tableName v
    , "insertable" .= tableInsertable v ]

data Column = Column {
  colSchema :: Text
, colTable :: Text
, colName :: Text
, colPosition :: Int64
, colNullable :: Bool
, colType :: Text
, colUpdatable :: Bool
, colMaxLen :: Maybe Int64
, colPrecision :: Maybe Int64
, colDefault :: Maybe Text
-- , colEnum :: Maybe [String]
-- , colFK :: Maybe ForeignKey
} deriving (Show)

instance JSON.ToJSON Column where
  toJSON c = JSON.object [
      "schema"    .= colSchema c
    , "name"      .= colName c
    , "position"  .= colPosition c
    , "nullable"  .= colNullable c
    , "type"      .= colType c
    , "updatable" .= colUpdatable c
    , "maxLen"    .= colMaxLen c
    , "precision" .= colPrecision c
    -- , "references".= colFK c
    , "default"   .= colDefault c
    -- , "enum"      .= colEnum c 
    ]

columnDecoder :: D.Row Column
columnDecoder = Column
  <$> D.column (D.nonNullable D.text) 
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.int8)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nullable D.int8)
  <*> D.column (D.nullable D.int8)
  <*> D.column (D.nullable D.text)

userDecoder :: D.Row User
userDecoder = User 
  <$> D.column (D.nonNullable D.text)
  <*> D.column (D.nullable D.text) 
  <*> D.column (D.nullable D.text)

getConnection :: IO (Maybe H.Connection)
getConnection = do
  conn <- H.Connection.acquire connectionSettings
  return $ case conn of
    Right c -> Just c
    Left _ -> Nothing
  where
    connectionSettings = H.Connection.settings "localhost" 5432 "" "" dbName
    dbName = "odoo-15-community"

foreignKeys :: Text -> Text -> H.Session (Vector (Text, Text, Text))
foreignKeys table schema = do
  H.Session.statement (table, schema) [TH.vectorStatement|
      select kcu.column_name :: text,
           ccu.table_name  :: text AS foreign_table_name,
           ccu.column_name :: text AS foreign_column_name
      from information_schema.table_constraints AS tc
             join information_schema.key_column_usage AS kcu
                  on tc.constraint_name = kcu.constraint_name
             join information_schema.constraint_column_usage AS ccu
                  on ccu.constraint_name = tc.constraint_name 
      where constraint_type = 'FOREIGN KEY'
        and tc.table_name = $1 :: text
        and tc.table_schema = $2 :: text
      order by kcu.column_name 
      |]

columns :: Text -> H.Session [Column]
columns t = do
  let query = "select info.table_schema as schema, info.table_name as table_name, \
      \       info.column_name as name, info.ordinal_position as position, \
      \       info.is_nullable as nullable, info.data_type as col_type, \
      \       info.is_updatable as updatable, \
      \       info.character_maximum_length as max_len, \
      \       info.numeric_precision as precision, \
      \       info.column_default as default_value, \
      \       array_to_string(enum_info.vals, ',') as enum \
      \   from ( \
      \     select table_schema, table_name, column_name, ordinal_position, \
      \            is_nullable, data_type, is_updatable, \
      \            character_maximum_length, numeric_precision, \
      \            column_default, udt_name \
      \       from information_schema.columns \
      \      where table_schema = $1 and table_name = $2 \
      \   ) as info \
      \   left outer join ( \
      \     select n.nspname as s, \
      \            t.typname as n, \
      \            array_agg(e.enumlabel) as vals \
      \     from pg_type t \
      \        join pg_enum e on t.oid = e.enumtypid \
      \        join pg_catalog.pg_namespace n ON n.oid = t.typnamespace \
      \     group by s, n \
      \   ) as enum_info \
      \   on (info.udt_name = enum_info.n) \
      \order by position"
  H.Session.statement (schema, t) $ H.Statement query encoder decoder True
  where
    schema = "public"
    encoder = contrazip2 (E.param (E.nonNullable E.text)) (E.param (E.nonNullable E.text))
    decoder = D.rowList columnDecoder

tables :: Text -> H.Session [Table]
tables schema = do
  let query = "select table_schema :: text as table_schema, \
      \table_name :: text as table_name, \
      \is_insertable_into as is_insertable_into \
      \from information_schema.tables \
      \where table_schema = $1 :: text and table_name like 'vp_%' \
      \order by table_name"
  H.Session.statement (schema) $ H.Statement query encoder decoder True
  where
    encoder = E.param (E.nonNullable E.text)
    decoder = D.rowList $ Table 
      <$> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.text) 
      <*> D.column (D.nonNullable D.bool)

primaryKeyColumns :: Text -> ReaderT H.Connection IO BL.ByteString
primaryKeyColumns t = do
  let query = "select kc.column_name \
      \  from \
      \    information_schema.table_constraints tc, \
      \    information_schema.key_column_usage kc \
      \where \
      \  tc.constraint_type = 'PRIMARY KEY' \
      \  and kc.table_name = tc.table_name and kc.table_schema = tc.table_schema \
      \  and kc.constraint_name = tc.constraint_name \
      \  and kc.table_schema = $1 \
      \  and kc.table_name  = $2"
  conn <- ask
  let statement = H.Session.statement 
                  (schema, t) 
                  (H.Statement query encoder decoder True)
  buf <- lift $ H.Session.run statement conn
  return $ case buf of
    Left _ -> "[]"
    Right val -> JSON.encode val
  where
    schema = "public" 
    encoder = contrazip2 (E.param (E.nonNullable E.text)) (E.param (E.nonNullable E.text))
    decoder = D.rowList $ D.column (D.nonNullable D.text)


tablesJsonData :: ReaderT H.Connection IO [Table]
tablesJsonData = do
  conn <- ask
  buf <- lift $ H.Session.run (tables "public") conn
  return $ case buf of
    Left _ -> mempty
    Right val -> val

columnJsonData :: Text -> ReaderT H.Connection IO BL.ByteString
columnJsonData t = do
  conn <- ask
  buf <- lift $ H.Session.run (columns t) conn
  return $ case buf of
    Left _ -> "[]"
    Right val -> JSON.encode val

tableRows :: Text -> Net.Query -> ReaderT H.Connection IO BL.ByteString
tableRows t qq = do
  conn <- ask
  let query = whereT qq $ asJsonRow t
  liftIO $ print query
  buf <- lift $ let 
                  stmt = H.Statement (cs query) encoder decoder True 
                in 
                  H.Session.run (H.Session.statement t stmt) conn
  case buf of
    Left e -> liftIO $ print e >> return "[]"
    Right val -> do
      case val of
        Just txt -> return (cs txt)
        Nothing -> return "[]" 
  where
    encoder = E.param (E.nonNullable E.text)
    decoder = D.rowMaybe (D.column (D.nonNullable D.text))



