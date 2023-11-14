{-# LANGUAGE DeriveGeneric #-}

module Github.Request where

import           qualified Prelude                      as P
import Relude
import Control.Monad.Trans.Writer
    ( execWriter, tell, Writer, WriterT )
import Control.Applicative ()


import qualified Data.Text                    as T
import qualified Data.ByteString.Builder as BS

import qualified Data.Aeson as J
import Network.HTTP.Simple
import Data.Aeson (FromJSON, ToJSON, (.:), eitherDecode, withObject)
import Network.HTTP.Conduit
import Network.HTTP.Types (status200)
import Network.HTTP.Client.TLS (setGlobalManager)

import qualified Data.ByteString.Lazy as LBS
import Data.String.Conversions (cs)

-- import GHC.Generics

-- StateT s IO
type VState = StateT Int IO

{- 
  lift :: (Monad m, MonadTrans t) => m a -> t m a
-}

basicState :: Int -> VState Int
basicState x = do
  prev <- get
  put (2 * prev + x)
  lift $ P.putStrLn "Double value and added input"
  return (x + prev)

stateWriterIO :: Int -> StateT Int (WriterT [String] IO) Int
stateWriterIO x = do
  prev <- get
  lift $ tell ["Get prev state"]
  put (2 * prev + x)
  liftIO $ P.putStrLn "Double value and added input"
  lift $ tell ["Returning input and previous value"]
  return (x + prev)

runST = runStateT $ basicState 1

lst = ZipList [(1+), (5*)] <*> ZipList [5,10]

testReader :: IO ()
testReader = do
  -- env <- getEnv
  -- t <- getLine @IO
  let val = runReader f1' 10
  print val

{- Semigroup, Monoid -}
testWriter :: IO ()
testWriter = do
  let val2 = execWriter w1'
  print val2

f1' :: Reader Int Int
f1' = do
  res <- f2'
  curVal <- ask
  return ((P.read (cs res) :: Int) * curVal)

f2' :: Reader Int String
f2' = do
    {- ask for val -}
    res <- ask
    return $ cs (intToByteString res)
  where
    intToByteString = LBS.toStrict . BS.toLazyByteString . BS.intDec

w1' :: Writer [String] ()
w1' = do
  tell ["hello"]
  _ <- w2'
  return ()

w2' :: Writer [String] ()
w2' = do
  tell ["world"]

ttt :: [(Integer, Integer)]
ttt = do
  a <- [1, 2, 3]
  b <- [4, 5, 6]
  return (a, b)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

tree :: Tree Char
tree = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

type WithCounter a = Int -> (a, Int)
{- This way, the type of relable becomes Tree a -> WithCounter (Tree a) -}

next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> let (r, i') = f i in g r i'
{- 
Passing and obtainingthe current index is quite tedious and, at the 
same time, has terrible consequences if it's wrong - if we
accidentally switch i1 and i2, this function would return the 
wrong results
-}

relable :: Tree a -> Int -> (Tree (Int, a), Int)
relable (Leaf x) i = (Leaf (i, x), i+1)
relable (Node l r) i = let (l', i1) = relable l i
                           (r', i2) = relable r i1
                        in (Node l' r', i2)

relable2 :: Tree b -> WithCounter (Tree (Int, b))
relable2 (Leaf x)   = \i -> (Leaf (i, x), i + 1)
relable2 (Node l r) = relable2 l `next` \l' ->
                      relable2 r `next` \r' ->
                      pure' (Node l' r')
                      where
                        pure' :: a -> WithCounter a
                        pure' x = \i -> (x, i)

incrCounter :: State Int Int
incrCounter = do n <- get
                 p <- put (n+1)
                 return (n+1)

getNumber :: IO (Maybe Int)
getNumber = readMaybe . T.unpack <$> getLine

getNumber' :: IO Int
getNumber' = do n <- getNumber
                case n of
                  Just m -> return m
                  Nothing -> getNumber'

showNumber' :: IO ()
showNumber' = do
  getNumber >>= \e -> case e of
    Just n -> P.putStrLn $ "Your number is " ++ show n
    Nothing -> fail "Pattern match failure in do expression"

ap' :: Monad m => m (b -> c) -> m b -> m c
ap' m1 m2 = do
  x1 <- m1
  x1 <$> m2


type S s a = s -> (a, s)

nextValue :: State Int Int
nextValue = do
  val <- get
  let nextVal = val + 1
  put nextVal
  modify (+10)
  return nextVal


t1 :: Reader Int Int
t1 = do
  val <- ask
  return val

t2 :: Reader Int String
t2 = do
  val <- t1
  return . show $ val

w1 :: Writer [Int] ()
w1 = do
  tell (pure 2)
  return () 

w2 :: Writer [Int] ()
w2 = do
  tell (pure 3)
  return ()

makeReader' :: Reader String Int
makeReader' = withReader P.read t1

testReader' :: IO ()
testReader' = do
  let val = runReader t2 1
  P.putStrLn $ val <> "Hello world"

data Expr = Lit Float | Devide Expr Expr

eval :: Expr -> Writer [String] Float
eval (Lit n) = return n
eval (Devide x y) = do
  x' <- eval x
  y' <- eval y
  case (x', y') of
    (_, 0) -> do tell ["Devide by zero"]
                 return 0
    (u, v) -> return (u / v)

mfilter' :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter' p x = do
  x' <- x
  if p x' then return x' else mzero

data GithubUser = GithubUser 
  { name :: String
  , followers :: Int
  } deriving (Generic, Show)

instance FromJSON GithubUser where
  parseJSON = withObject "GithubUser" $ \v ->
    GithubUser
      <$> v .: "name"
      <*> v .: "followers"

instance ToJSON GithubUser where
  toEncoding = J.genericToEncoding J.defaultOptions

newtype CatBoy = CatBoy 
  { url :: String } deriving (Generic, Show)

instance FromJSON CatBoy where
  parseJSON = withObject "CatBoy" $ \v ->
    CatBoy <$> v .: "url"

instance ToJSON CatBoy where
  toEncoding = J.genericToEncoding J.defaultOptions

getUserData :: String -> IO (Either String GithubUser)
getUserData username = do
  manager <- newManager tlsManagerSettings
  setGlobalManager manager

  initialRequest <- parseRequest $ "https://api.github.com/users/" <> username
  let request = initialRequest 
        { method = "GET"
        , requestHeaders = 
          [ ("Content-Type", "application/json")
          , ("User-Agent", "5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36")]
        } 
  response <- httpLBS request
  
  if getResponseStatus response /= status200
    then return $ Left $ cs $ getResponseBody response
    else do
      let body = getResponseBody response
      return $ eitherDecode body

jsonUserFromGithub :: String -> IO ()
jsonUserFromGithub username = do
  user <- getUserData username
  case user of
    Right u -> P.print $ followers u
    Left e -> putStrLn e

