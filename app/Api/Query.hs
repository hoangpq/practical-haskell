{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Query where

import Data.Text hiding (map, empty)
import qualified Network.HTTP.Simple as Net
import Data.String.Conversions (cs)
import Data.Maybe (fromMaybe)
import qualified Data.List as L

asJsonRow :: Text -> Text
asJsonRow s = "select array_to_json(array_agg(row_to_json(t)))::character varying \
              \from " <> s <> " t"


wherePred :: Net.QueryItem -> Text
wherePred (col, predicate) = cs col <> " " <> op <> " " <> cs value
  where
    opCode:rest = split (=='.') $ cs $ fromMaybe "." predicate
    value = intercalate "." rest
    op = case opCode of
      "eq"  -> "="
      "gt"  -> ">"
      "lt"  -> "<"
      "gte" -> ">="
      "lte" -> "<="
      "neq" -> "<>"
      _     -> "="

whereT :: Net.Query -> Text -> Text
whereT params q = 
  if L.null cols
    then q
    else q <> " where " <> conjunction
  where
    cols = [ col | col <- params, fst col /= "orders" ]
    conjunction = mconcat $ L.intersperse " and " (map wherePred cols)
