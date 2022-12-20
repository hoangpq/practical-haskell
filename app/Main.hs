{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where


import           qualified Prelude                      as P
import Relude

import qualified System.Directory             as Dir
import           System.FilePath              ((</>))


import qualified System.IO                    as IO

import           Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate,
                                               runResourceT)

import qualified Control.Exception.Safe       as Ex
import qualified Data.Char                    as C


import qualified Data.Text                    as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.IO                 as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS

import qualified Data.Text.Encoding as T

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S

import qualified ASCII as A
import qualified ASCII.Char as A

import Network.Simple.TCP (serve, HostPreference (..))
import qualified Network.Simple.TCP as Net

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time as Time

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as Html


import Data.Time.Clock.POSIX (getPOSIXTime)

import List.Transformer (ListT, runListT)
import qualified List.Transformer as ListT
import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy as BSB

import qualified Data.Attoparsec.ByteString as Parser
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec as Parser

import qualified Data.List as List
import qualified Data.ByteString as List
import qualified Data.Char as Char
import Control.Monad.Trans.Writer
    ( execWriter, tell, Writer, WriterT )
import Control.Applicative ()

import Data.String.Conversions (cs)
import GHC.Show (showMultiLineString)

import qualified Data.ByteString.Lazy.Char8 as LBS1

import System.IO.Unsafe
import qualified Data.ByteString as LBS1
import qualified Relude as Data.ByteString

import Control.Monad.Error.Class (catchError)

import qualified HsLua as L
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import qualified Control.Monad.ST.Safe as ST
import Control.Concurrent.STM (modifyTVar)
import Control.Monad.Trans.Control
import HsLua (NumResults(NumResults), resultToEither)

import qualified Control.Concurrent as CC
import System.Random hiding (next)
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception
import qualified Hasql.Session as H.Session

import Api.Structure (getConnection, tables, foreignKeys)
import qualified Control.Concurrent.Async as Async

import Network.Wai.Handler.Warp (run, setPort, defaultSettings)
import Api.Server (app)

import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)

import Control.Lens
import qualified Data.Aeson as J

import Github.Request (eval, getJSONData)
import Github.Request (followers)

-- foreign import ccall "example_create" createExample
--   :: IO (Ptr Example)
-- foreign import ccall "&example_destroy" destroyExample
--   :: FunPtr (Ptr Example -> IO ())
-- foreign import ccall "example_do_stuff" doStuffExample
--   :: Ptr Example -> CInt -> IO CBool

-- data Example

-- mkExample :: IO (ForeignPtr Example)
-- mkExample = mask_ $ do
--   -- NOTE: mask_ is needed to avoid leaking memory between
--   -- allocating the struct and wrapping the `Ptr` in a `ForeignPtr`.
--   ptr <- createExample
--   newForeignPtr destroyExample ptr

-- testZigFFi :: IO ()
-- testZigFFi = do
--   ptr <- mkExample
--   withForeignPtr ptr \p -> print =<< doStuffExample p 42

getDataDir :: IO FilePath
getDataDir = do
  dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
  Dir.createDirectoryIfMissing True dir
  return dir

-- helloWorld = IO.putStrLn "hello world"
-- helloHandle = IO.hPutStrLn IO.stdout "hello world!"

writeGreetingFile :: IO ()
writeGreetingFile = do
  dir <- getDataDir
  h <- IO.openFile (dir </> "greeting.txt") WriteMode
  IO.hPutStrLn h "hello"
  IO.hPutStrLn h "world"
  IO.hClose h

helloWorld :: MonadIO m => m ()
helloWorld = liftIO (IO.putStrLn "hello world!")

fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
  Just h <- fileResourceMaybe
  liftIO (IO.hPutStrLn h "hello")
  liftIO (IO.hPutStrLn h "world")
  liftIO (IO.hPutStrLn h "from")
  liftIO (IO.hPutStrLn h "Vampire")

fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
  dir <- liftIO getDataDir
  result <- Ex.tryIO do
    (_, handle) <- fileResource (dir </> "greeting.txt") WriteMode
    return $ Just handle
  case result of
    Right x -> return x
    Left e -> do
      print (displayException e)
      return Nothing

howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
  hs <- openManyHandles
  putStrLn ("Opened " <> show (length hs) <> " handles")

openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
  dir <- liftIO getDataDir
  let paths = [dir </> "greeting" <> show i <> ".txt" | i <- [1 .. 10]]
  traverse (\path -> snd <$> fileResource path WriteMode) paths

printCapitalizedText :: Handle -> IO ()
printCapitalizedText h = continue
  where
    continue = do
      chunk <- T.hGetChunk h
      case (T.null chunk) of
        True -> return ()
        False -> do
          T.putStr (T.toUpper chunk)
          continue

repeatUntilIO :: Monad m => m t -> (t -> Bool) -> (t -> m a) -> m ()
repeatUntilIO getChunk isEnd f = continue
  where
    continue = do
      chunk <- getChunk
      unless (isEnd chunk) do {
          _ <- f chunk; continue
        }

printFileContentUpperCase :: IO ()
printFileContentUpperCase = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO (printCapitalizedText h)

printFileContentUpperCase2 :: IO ()
printFileContentUpperCase2 = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <-
    fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ repeatUntilIO (T.hGetChunk h) T.null (T.putStr . T.toUpper)

digitsOnly :: Text -> Text
digitsOnly = T.filter C.isDigit

hello :: MonadIO m => m ()
hello = liftIO (IO.putStrLn "hello world!")

helloText :: IO ()
helloText = T.hPutStrLn stdout (T.pack "hello world!")

capitalizeLast :: Text -> Text
capitalizeLast text =
  T.map (\c -> if c == l then C.toUpper c else c) text
    where
      l = T.last text

unParen :: Text -> Maybe Text
unParen text =
  let
    filtered = T.filter (not . isParen) text
  in
    if filtered /= text && isPair text
      then Just filtered
      else Nothing
  where
    isParen t = t == '(' || t == ')'
    isPair t = (== '(') (T.head t) && (== ')') (T.last t)

characterCount :: FilePath -> IO Int
characterCount fp = do
  dir <- getDataDir
  x <- T.readFile (dir </> fp)
  return (T.length x)


characterCount2 :: FilePath -> IO Int
characterCount2 fp = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> fp) ReadMode
  liftIO (ccount h)
  where
    ccount :: Handle -> IO Int
    ccount h = continue 0
      where
        continue t = do
          chunk <- T.hGetChunk h
          if T.null chunk
            then return t
            else do continue(t + T.length chunk)

-- helloText = T.hPutStrLn IO.stdout "hello world!"
-- t = take 10 $ R.cycle "abc"

-- copy content from file to file
copyGreetingFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h1) <- 
    binaryFileResource (dir </> "greeting.txt") ReadMode
  (_, h2) <- 
    binaryFileResource (dir </> "greeting2.txt") WriteMode
  liftIO $ do 
    repeatUntilIO (BS.hGetSome h1 1024) BS.null \chunk -> 
      BS.hPutStr h2 chunk
    BS.hPutStr h2 $ BS.pack [104, 101, 108, 108, 111]

-- open file in binary mode
binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode =
  allocate (IO.openBinaryFile path mode) IO.hClose

helloByteString = do
  IO.hSetBinaryMode stdout True
  BS.hPut stdout (BS.pack helloBytes)

helloBytes = [
  104, 101, 108, 108, 111,      -- hello
  32,                           -- space
  119, 111, 114, 108, 100, 33,  -- world!
  10 ]                          -- \n

helloUtf8 = do
  IO.hSetBinaryMode stdout True
  BS.hPutStr stdout (T.encodeUtf8 (T.pack "hello world!\n"))

greet :: BS.ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
  Left _ -> P.putStrLn "Unvalid byte string"
  Right nameText -> T.putStrLn (T.pack "Hello, " <> nameText)

asciiUpper :: BS.ByteString -> BS.ByteString
asciiUpper = BS.map (\w -> if w >= 92 then w - 32 else w)

makeFriend :: S.SockAddr -> IO ()
makeFriend address = do
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.connect s address
  S.sendAll s $ T.encodeUtf8 $
    T.pack "Hello, will you be my friend?"
  repeatUntilIO (S.recv s 1024) BS.null BS.putStr

sockBind :: S.SockAddr -> IO ()
sockBind address = do
  s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
  liftIO do
    S.bind s address
    S.listen s S.maxListenQueue
    _ <- receive s
    S.close s

  where 
    receive s = do
      (c, _) <- S.accept s
      buf <- S.recv c 1024
      unless (nullCheck buf) do
        BS.putStr buf
        receive s
      S.close c
    nullCheck = BS.null . strip
    strip = BS.filter (/= 10)


line :: BS.ByteString -> BS.ByteString
line x = x <> fromString "\r\n"

helloRequestString :: BS.ByteString
helloRequestString = 
  line (fromString "GET / HTTP/1.1") <>
  line (fromString "User-Agent: curl/7.16.3") <>
  line (fromString "Host: book.realworldhaskell.org") <>
  line (fromString "Accept-Language: en") <>
  line (fromString "")

makeSocket :: S.Socket -> IO ()
makeSocket s = do
  S.sendAll s helloRequestString
  repeatUntilIO (S.recv s 1024) BS.null BS.putStr 
  {- ... connect, send, listen, etc. ... -}
  S.gracefulClose s 1000

makeFriendSafely :: S.SockAddr -> IO ()
makeFriendSafely addr = runResourceT @IO do
  (_, s) <- allocate 
    (S.socket S.AF_INET S.Stream S.defaultProtocol)
    S.close
  liftIO $ do
    S.connect s addr
    makeSocket s

makeFriendAddrInfo :: S.AddrInfo -> IO ()
makeFriendAddrInfo addressInfo = runResourceT @IO do
  (_, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO $ do
    S.connect s (S.addrAddress addressInfo)
    makeSocket s

testCall = makeFriendSafely (S.SockAddrInet 22 (S.tupleToHostAddress (103, 110, 84, 163)))

connectToHaskellWebsite = makeFriendSafely (S.SockAddrInet 80 (S.tupleToHostAddress (147, 75, 54, 133)))

helloRequestString2 = 
  line [A.string| GET / HTTP/1.1|]

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
  addrInfos <- S.getAddrInfo 
    (Just S.defaultHints { S.addrSocketType = S.Stream }) 
    (Just "book.realworldhaskell.org") 
    (Just "http")
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

helloResponseString = 
  line [A.string|HTTP/1.1 200 OK|] <>
  line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
  line [A.string|Content-Length: 6|] <>
  line [A.string||] <>
  [A.string|Hello!|]

helloResponseStringChunked = 
  line [A.string|HTTP/1.1 200 OK|] <>
  line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
  line [A.string|Transfer-Encoding: chunked|] <>
  line [A.string||] <>
  line [A.string|2|] <> line [A.string|He|] <>
  line [A.string|4|] <> line [A.string|llo!|]

sayHelloWithBuilder :: T.Text -> T.Text
sayHelloWithBuilder name = LT.toStrict $ TB.toLazyText $ 
  TB.fromString "Hello " <> 
  TB.fromText name <>
  TB.fromString "!"

sayHello :: T.Text -> T.Text
sayHello name = T.pack "Hello, " <> name <> T.pack "!"

time :: IO () -> IO ()
time action = do
  a <- Time.getCurrentTime
  action
  b <- Time.getCurrentTime
  P.print (Time.diffUTCTime b a)

concatWithStrict :: Int -> T.Text
concatWithStrict numberOfTimes =
  fold $ replicate numberOfTimes $ T.pack "a"

concatWithBuilder :: Int -> T.Text
concatWithBuilder numberOfTimes = LT.toStrict $ TB.toLazyText $ 
  fold $ replicate numberOfTimes $ TB.fromString "a"

concatSpeedTest :: Int -> IO ()
concatSpeedTest n = do
  dir <- getDataDir
  time $ T.writeFile (dir </> "strict.txt") 
                     (concatWithStrict n)
  time $ T.writeFile (dir </> "builder.txt") 
                     (concatWithBuilder n)

toHtmlString :: LBS.ByteString
toHtmlString = renderHtml $ Html.html (Html.head $ Html.title "Vampire")

increment :: TVar Natural -> STM Natural
increment hitCounter = do
  oldCount <- readTVar hitCounter
  let newCount = oldCount + 1
  writeTVar hitCounter newCount
  return newCount

testIncrement :: (TVar Natural -> IO a) -> IO Natural
testIncrement inc = do
  x <- atomically (newTVar @Natural 0)
  Async.replicateConcurrently_ 10 (replicateM 1000 (inc x))
  atomically (readTVar x)

newtype MaxChunkSize = MaxChunkSize Int

testListTransformer :: IO ()
testListTransformer = runListT (do
    n <- ListT.select [0 :: Int ..]
    liftIO (print n))

list1 = ["chicken", "first"]
list2 = ["soup", "salad"]

haskellCafe :: [(String, String)]
haskellCafe = do
  entree <- list1
  side <- list2
  return (entree, side)

haskellCafev2 :: [(String, String)]
haskellCafev2 = list1 >>= 
  \x -> list2 >>= 
    \y -> [(x, y)]

printMenu :: [(String, String)] -> IO ()
printMenu meals = runListT @IO do
  (entree, side) <- ListT.select @[] meals
  liftIO (IO.putStrLn (entree <|> " and " <|> side))

metronome = runListT @IO do
  ListT.select @[] (repeat ())
  liftIO (IO.putStrLn "tick")
  liftIO (threadDelay 1000000)

getHandle :: IO Handle
getHandle = do
  dir <- liftIO getDataDir
  h <- IO.openFile (dir </> "greeting.txt") ReadMode
  return h

infiniteChunks = runListT @IO do
  h <- liftIO getHandle
  ListT.select @[] (repeat ())
  bs <- liftIO $ BS.hGetSome h 1024
  liftIO $ BS.putStr bs
  liftIO (threadDelay 1000000)

selectChunk :: BS.ByteString -> ListT IO BS.ByteString
selectChunk bs = 
  ListT.select @[] (BSB.toChunks (BS.toLazyByteString (BS.byteString bs)))

ourFirstServer = serve @IO HostAny "8000" \(s, a) -> do
  putStrLn ("New connection from " <> P.show a)
  t <- sendTime
  let buf = helloResponseStringChunked <> 
            t <> 
            line [A.string|0|] <> line [A.string||]
  {-- Send data back to client --}
  putStrLn $ show buf

  Net.send s buf
  where
    sendTime = do
        -- t <- (round . (* 1000) <$> getPOSIXTime)
        let ts = "\9835"
        return $ line (intToByteString $ BS.length ts) <> line ts
      where
        intToByteString = LBS.toStrict . BS.toLazyByteString . BS.intDec

countString :: BS.ByteString
countString = [A.string|one-1-two-three-four|]

digitParser :: Parser.Parser A.Digit
digitParser = do
  x <- Parser.anyWord8
  unless (A.isDigit x) (fail "0-9 expected")
  return (A.word8ToDigitUnsafe x)

runParser = Parser.parseOnly (digitParser <?> "parser digit") countString

openManyHandles2 :: ResourceT IO [Handle]
openManyHandles2 = go []
  where
    go hs = do
      r <- fileResourceMaybe2
      case r of
        Nothing -> return hs
        Just h -> go (h:hs) 

fileResourceMaybe2 = do
  dir <- liftIO getDataDir
  result <- Ex.tryIO do
    (_, h) <- fileResource (dir <> "/greeting.txt") ReadMode
    return (Just h)
  case result of
    Right x -> return x
    Left e -> do
      print (displayException e)
      return Nothing

howManyFileOpen = do
  val <- openManyHandles2
  putStrLn $ show $ P.length val

capitalizeLast2 t = case T.unsnoc t of
  Just (t', c) -> T.snoc t' (Char.toUpper c)
  Nothing -> T.empty

asciiUpper2 = BS.map f
  where
    f x | x >= 97, x <= 122 = x - 32
    f x = x

{- Functor -}
{- Applicative Functor -}


type Person = String

people :: [Person]
people = ["Alejandro", "Elena", "Quique", "John", "Mary", "Tom"]

pcRels :: [(Person, Person)]
pcRels = [("Alejandro", "Quique"), ("Elena", "Quique")
         ,("John", "Mary"), ("John", "Tom"), ("Mary", "Tim")]

gpgcRels :: [(Person, Person, Person)]
gpgcRels = do
  (grandp, parent) <- pcRels
  (parent', grandc) <- pcRels
  guard (parent == parent')
  return (grandp, parent, grandc)

vals' = [1, 2, 3]

test'' :: [(Int, Int, Int)]
test'' = do
  x <- vals'
  y <- vals' 
  guard (y /= x)
  z <- vals'
  guard (x /= y)
  guard (x + y == z)
  return (x, y, z)

weirdSum = do
  x <- newSTRef 1
  y <- newSTRef 1
  modifySTRef y (+1)
  (+) <$> readSTRef x <*> readSTRef y

test''' :: (Monoid a, Fractional a, Eq a, Show a) => a -> a -> Maybe a
test''' x y = do
    go `catchError` \_ -> empty
  where
    go = if y /= 0 then pure (x / y) else mempty

addName :: TVar Integer -> TVar [(Integer, String)] -> String -> STM ()
addName counter names n = do
  i <- readTVar counter
  modifyTVar names ((i, n) :)
  writeTVar counter (i  + 1)

testTVar = do
  counter <- newTVar 1
  names <- newTVar []
  -- _ <- atomically (addName counter names "Vampire")
  return ()

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x: r@(y:_)) = x < y && sorted r


maxmin [x] = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

ifibonnaci :: Integer -> Maybe Integer
ifibonnaci n = if n < 0
               then Nothing
               else case n of
                     0 -> Just 0
                     1 -> Just 1
                     n -> let Just f1 = ifibonnaci (n-1)
                              Just f2 = ifibonnaci (n-2)
                          in Just (f1 + f2)

data Client = Client { fName :: String }

fibonacci :: Int -> Int
fibonacci n = fibs P.!! n
  where fibs = 0 : 1 : zipWith (+) fibs (P.tail fibs)

specialClient :: Client -> Bool
specialClient (fName -> "Vampire") = True
specialClient p@Client { fName = _ } = length (fName p) > 10

allNumberFrom :: Integer -> [Integer]
allNumberFrom n = n : allNumberFrom (n+1)

fibonacci2 :: [Integer]
fibonacci2 = map fst $ iterate (\(n,n1) -> (n1,n+n1)) (0,1)

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
              where sumForce' [] z = z
                    sumForce' (!y:ys) !z = sumForce' ys (y + z)

lenText :: String
lenText = "abc"

updateMoney :: MVar Integer -> IO ()
updateMoney v = do
  m <- takeMVar v
  putStrLn $ "Updating value, which is " ++ show m
  putMVar v (m + 500)

randomDelay :: IO ()
randomDelay = do
  r <- randomRIO (3, 15)
  threadDelay (r * 1000000)

smt = do
  v <- newMVar 10000
  _ <- CC.forkIO $ updateMoney v
  _ <- CC.forkIO $ updateMoney v
  _ <- CC.forkIO $ updateMoney v
  _ <- getLine
  return ()

runLuaVM = L.run @L.Exception $ do
  hSetBuffering stdout LineBuffering
  L.openlibs
  let fibonnaci' :: L.LuaError e => L.LuaE e L.NumResults
      fibonnaci' = do
        l <- L.state
        Just num <- L.tointeger 1
        let f = fromIntegral $ fibonacci (fromIntegral num)
        L.pushinteger f
        return (NumResults 1)

  L.pushHaskellFunction fibonnaci' *> L.setglobal "fibonacci"
  void . L.dostring $ mconcat 
    [ "print(fibonacci(100))\n"
    , "print('world')\n"
    ]

-- A value of TState s a should be understood as
-- a computation that produces values of type a 
-- while modifying an internal state of type s
-- In fact, we can see State s a as the encoding
-- of functions from an initial state to their
-- return value, paired with the new state'

-- haskell implementation warning
newtype TState s a = TState { runTState :: s -> (a, s) }

instance Functor (TState s) where
  fmap f (TState x) = TState \s -> let (a, s') = x s in (f a, s')

instance Applicative (TState s) where
  pure x = TState $ \s -> (x, s)
  m <*> n = TState $ \s ->
    case runTState m s  of { (f, s') -> 
    case runTState n s' of { (x, s'') -> 
                             (f x, s'') }}

instance Monad (TState s) where
  m >>= n = TState $ \s -> case runTState m s of
    (r, s') -> runTState (n r) s'

nextValue :: TState Int Int
nextValue = TState (\i -> (i, i + 1))

getT :: TState s s 
getT = TState $ \s -> (s, s)

evalTState :: TState s a -> s -> a
evalTState act = fst . runTState act

benchWsApp :: TMVar.TMVar () -> WS.ServerApp
benchWsApp _ pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $
    forever $ do
      _ <- WS.receive conn
      fork_ $ do
        eU <- getJSONData
        case eU of
          Right u -> WS.sendTextData conn (J.encode u)
          _ -> WS.sendTextData conn (T.pack "()")
  where
    followers_ = T.pack . show . followers
    fork_ = void . forkIO

runWs :: IO ()
runWs = do
  lock <- TMVar.newEmptyTMVarIO
  P.putStrLn "Running wrk websocket server or port 9160"
  WS.runServer "0.0.0.0" 9160 $ benchWsApp lock

main = runWs

{-
main :: IO ()
main = do
  Just conn <- getConnection
  run 4000 (app conn)
-}
