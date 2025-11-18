{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lessons.Lesson10 where

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

import Control.Monad
import Data.Foldable

import Control.Applicative


import Test.QuickCheck (Arbitrary, arbitrary, quickCheckResult)
import Test.QuickCheck.Gen

import Data.Char

type ErrorMsg = String
type Input = String
type Parser a = ExceptT ErrorMsg (State Input) a -- when i write parser a i mean this
-- exceptT lets exit parsing when mistake occurs
-- exceptt turi arba errormsg arba state input pakoreguota ir a
-- parser which can throw error and as remaining input as state
-- exceptT - monad transformer 
-- ExceptT ErrorMsg (State Input) a -> State Input (Either ErrorMsg a)
-- -> Input -> (Either ErrorMsg a, Input)

parseLetter :: Parser Char
parseLetter = do 
    input <- lift get -- get takes input, lift is used because state -> exceptt
    case input of
        [] -> throwE "A letter is expected but got empty input" -- throwE is ExceptT constructot
        (h:t) -> if isAlpha h
            then do
                lift (put t) -- updates state text, put - changes state to given text
                return h -- monadic return
            else throwE $ "A letter is expected, but got " ++ [h]

parseTwoLetters :: Parser String
parseTwoLetters = do
    a <- parseLetter
    b <- parseLetter
    pure [a, b]

-- applicative
parseTwoLetters' :: Parser String
parseTwoLetters' = (\a b -> [a, b]) <$> parseLetter <*> parseLetter

-- do notation - used on monad instance
-- applicative - functor (<$>) and applicative (<*>) instances
-- alternative - many, some and <|>

-- >>> :t runExceptT parseTwoLetters
-- runExceptT parseTwoLetters :: State Input (Either ErrorMsg String)

-- >>> :t runState (runExceptT parseTwoLetters)
-- runState (runExceptT parseTwoLetters) :: Input -> (Either ErrorMsg String, Input)

-- >>> parse parseTwoLetters ""
-- (Left "A letter is expected but got empty input","")
-- >>> parse parseTwoLetters "ds"
-- (Right "ds","")
-- >>> parse parseTwoLetters "545435"
-- (Left "A letter is expected, but got 5","545435")

parse :: Parser a -> Input -> (Either ErrorMsg a, Input)
parse p = runState (runExceptT p)
-- runState takes away the state ??
-- runExceptT takes away the ExceptT ??

-- type creates name for already existing type (in this case for a)
-- new type - single constructor
-- data - many constructors
type Weird a = ExceptT Int (StateT String IO) a

weird :: Weird Double
weird = do
    lift $ lift $ putStrLn "Hello?" -- lift IO -> State, lift State -> ExceptT
    answer <- lift $ lift $ getLine -- lift - perkelia i isorine monada
    lift $ put answer -- put jau ne IO o state, lift state -> Except
    return 3.14 -- 3.14 ipakuotas i monadu kruva

-- >>> :t weird
-- weird :: Weird Double
-- >>> :t runExceptT weird
-- runExceptT weird :: StateT String IO (Either Int Double)
-- >>> :t runStateT (runExceptT weird) "fsd"
-- runStateT (runExceptT weird) "fsd" :: IO (Either Int Double, String)

weird' :: Weird Double
weird' = do
    liftIO $ putStrLn "Hello?" 
    answer <- liftIO $ getLine
    lift $ put answer
    return 3.14

-- ADT
data SomeData = Foo String | Bar Integer deriving Show

-- >>> generate arbitrary :: IO SomeData
instance Arbitrary SomeData where
  arbitrary :: Gen SomeData 
  arbitrary = oneof [fmap Foo arbitrary, fmap Bar arbitrary]
  -- generate random string - put it in a foo
  -- generate random int - put it in a bar
