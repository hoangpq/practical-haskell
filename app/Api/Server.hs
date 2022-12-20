module Api.Server where

import Network.Wai
-- import Network.Wai.Internal

import Network.HTTP.Types.Status
-- import Network.HTTP.Types.Header
-- import Network.HTTP.Types.URI

-- import Data.String.Conversions (cs)
import Hasql.Connection as H.Connection
import Control.Monad.Trans.Reader

import Api.Structure (primaryKeyColumns, columnJsonData, tableRows)

app :: Connection -> Application
app conn req respond = do
  -- putStrLn $ "Request: " ++ show req
  respond =<< case (path, verb) of
    ([t], "GET") -> do
      responseLBS status200 [] <$> runReaderT (tableRows t qq) conn
    (["primary", t], "GET") -> responseLBS status200 [] <$> runReaderT (primaryKeyColumns t) conn
    (["column", t], "GET") -> responseLBS status200 [] <$> runReaderT (columnJsonData t) conn 
    (_, _) -> return $ responseLBS status404 [] ""
  where
    path = pathInfo req
    verb = requestMethod req
    qq = queryString req
