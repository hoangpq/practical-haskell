{-# 
    LANGUAGE 
    DeriveGeneric, DeriveAnyClass, 
    ScopedTypeVariables, TemplateHaskell, 
    DerivingVia, DataKinds
#-}
module People where

import GHC.Generics
import Data.Hashable

import Data.Text (Text)
import qualified Data.Text as T

import Data.List (intersperse)
import Data.Text.Lazy.Builder (Builder)

import Text.Regex.TDFA hiding (match)
import Text.Regex.Applicative

import Data.IORef
import Data.STRef

import GHC.ST

import Data.Vector (Vector, thaw, freeze)
import qualified Data.Vector as V
-- mutable vector
import qualified Data.Vector.Mutable as M

-- should be explore
import Control.Monad

import Data.Aeson
import Data.Aeson.Types hiding (Parser)
import Data.Aeson.TH

import Deriving.Aeson.Stock
import Data.Binary

import Data.List.NonEmpty hiding (drop, intersperse)
import Data.Validation as Va

-- to make this work for HS.singleton, 
-- you need that Hashable instance
data Person = Person { name :: String, age :: Int } 
            deriving (Eq, Show, Generic, Hashable)

instance ToJSON Person where
    toJSON (Person name age) = object [ "name" .= name, "age" .= age ]

instance FromJSON Person where
    parseJSON (Object keymap) = Person <$> keymap .: "name" 
                                       <*> keymap .: "age"
    parseJSON invalid = typeMismatch "Person" invalid

data Person2 = Person2 { personName :: String, personAge :: Int } 
    deriving (Eq, Show, Generic)

deriveJSON defaultOptions {
    fieldLabelModifier = (("person_" <>) . (drop 6)),
    rejectUnknownFields = True
} ''Person2

data Person3 = Person3 { person2Name :: Text, person2Age :: Int }
    deriving (Generic, Binary)
    deriving (ToJSON, FromJSON) via PrefixedSnake "person2" Person3

greet :: Text -> Text
greet name = "Hello, " <> name <> "!" 

greetBuilder :: [Text] -> Text
greetBuilder names = "hello to " <> mconcat (intersperse ", " names) <> "!"

testRegex :: Text -> Maybe Text
testRegex s = 
    s =~~ regex
    where
        regex = "[0-9]+" :: Text

-- Text.Regex.Applicative
testApplicativeRegex :: [Char] -> Maybe [Char] 
testApplicativeRegex s = 
    let number = many $ psym (\x -> x `elem` ['0' .. '9'])
    in match number s

-- Data.Vector
testVector = (V.singleton 1, V.replicate 3 1)

-- data Point = Point {-# UNPACK #-} !Int {-# UNPACK #-} !Int

listLength :: [a] -> IO Int
listLength xs = do
    l <- newIORef 0
    listLengthHelper l xs
    readIORef l

listLengthHelper :: IORef Int -> [a] -> IO ()
listLengthHelper _ [] = pure ()
listLengthHelper l (_:xs) = do
    modifyIORef' l (+1)
    listLengthHelper l xs

listLengthHelperST :: STRef s Int -> [a] -> ST s ()
listLengthHelperST _ [] = pure ()
listLengthHelperST l (_:xs) = do
    modifySTRef' l (+1)
    listLengthHelperST l xs

listLengthST :: [a] -> Int
listLengthST xs = runST $ do
    l <- newSTRef 0
    listLengthHelperST l xs
    readSTRef l

bubbleSort :: Ord a => Vector a -> Vector a
bubbleSort xs = runST $ do
    elts <- thaw xs
    mapM_ (go elts) ([1 .. M.length elts - 1] :: [Int])
    freeze elts
    where
        go elts 0 = pure ()
        go elts n = do
            -- read values in those positions
            x <- M.read elts (n-1)
            y <- M.read elts n
            -- if needed, swap and keep trying
            when (y < x) $ do
                M.swap elts (n-1) n
                go elts (n-1)

testBubbleSort :: Vector Int
testBubbleSort = bubbleSort $ V.fromList [1, 3, 2, 5, 4]

-- Applicative and Alternative
-- validPerson :: String -> String -> Int -> Maybe Person
-- validPerson first last age
--     | null first = Nothing
--     | null last = Nothing
--     | age < 0 = Nothing
--     | otherwise = Just (Person (Name first last) age)

notEmpty :: String -> Maybe String
notEmpty "" = Nothing
notEmpty s = Just s

nonNegative :: Int -> Maybe Int
nonNegative n | n < 0 = Nothing
              | otherwise = Just n

validPerson :: String -> Int -> Maybe Person
validPerson name age = 
    Person <$> (notEmpty name) <*> (nonNegative age)

notEmptyE :: String -> Either String String
notEmptyE "" = Left "empty string"
notEmptyE s = Right s

notEmptyV :: String -> Validation (NonEmpty String) String
notEmptyV = validationNel . notEmptyE

nonNegativeV :: Int -> Validation (NonEmpty String) Int
nonNegativeV age
    | age <= 0 = Va.Failure $ "nagative number" :| []
    | otherwise = Va.Success age

validPersonV :: String -> Int -> Validation (NonEmpty String) Person
validPersonV name age = 
    Person <$> (notEmptyV name) 
           <*> (nonNegativeV age)

test = do
    case validPersonV "" (-1) of
        Va.Success value -> putStrLn $ show value
        Va.Failure error -> putStrLn $ show error
