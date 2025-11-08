{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lessons.Lesson09 where

import Lessons.Lesson08(Parser(..), threeLetters)

import Control.Applicative

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

import Control.Monad
import Data.Foldable


-- | sequence - monad, sequenceA - Applicative
-- mplus - monad with alternatives
-- treversable - list, maybe is traversable
-- foldable - foldl, foldr, fold. Go through all elements and combine them into one value.
-- >>> sequenceA [Just 42, Just 5]
-- Just [42,5]

-- >>> sequenceA [Just 42, Just 5, Nothing]
-- Nothing

-- >>> sequenceA [[42], [13, 45]]
-- [[42,13],[42,45]]

-- >>> sequenceA [[42], [13, 45], []]
-- []

-- >>> sequenceA [Right 43, Right 45]
-- Right [43,45]

-- >>> sequenceA [Left 0, Right 43, Right 45]
-- Left 0

-- >>> sequenceA $ Right (Right 45)
-- Right (Right 45)

-- >>> sequenceA $ Right (Right 45)
-- Right (Right 45)-- Left 45

-- >>> sequenceA $ Just (Right 45)
-- Right (Just 45)

-- >>> :t sequenceA [getLine, getLine, getLine]
-- sequenceA [getLine, getLine, getLine] :: IO [String]


-- list of IO actions -> IO action returning list of results
-- >>> :t [getLine, getLine, getLine]
-- [getLine, getLine, getLine] :: [IO String]

-- read wraps String into IO String
-- returns IO action that when executed will read a line from console
queryAge :: String -> IO Integer
queryAge name = do
    putStrLn $ "What is your age, " ++ name ++ "?"
    read <$> getLine

-- >>> :t mapM queryAge ["VI", "A", "U"]
-- mapM queryAge ["VI", "A", "U"] :: IO [Integer]

-- >>> :t map queryAge ["VI", "A", "U"]
-- map queryAge ["VI", "A", "U"] :: [IO Integer]

-- >>> :t mapM_ queryAge ["VI", "A", "U"]
-- mapM_ queryAge ["VI", "A", "U"] :: IO ()

-- underscore version ignores the result
-- function acts as a way to start computation with side effects, does not bring any result
-- IO ()
-- >>> :t forM_ ["VI", "A", "U"] queryAge
-- forM_ ["VI", "A", "U"] queryAge :: IO ()

-- lifts function to monadic function
-- >>> liftM2 (+) (Just 4) (Just 6)
-- Just 10

-- >>>  (+) <$> (Just 4) <*> (Just 6)
-- Just 10

-- >>> liftM3 (\a b c -> a + b - c) (Just 4) (Just 6) Nothing
-- Nothing

-- >>> liftA3 (\a b c -> a + b - c) (Just 4) (Just 6) Nothing
-- Nothing

-- >>> :t threeLetters
-- threeLetters :: Parser String

-- >>> :t sequenceA [threeLetters, threeLetters, threeLetters]
-- sequenceA [threeLetters, threeLetters, threeLetters] :: Parser [String]

-- >>> :t runParser (sequenceA [threeLetters, threeLetters, threeLetters])
-- runParser (sequenceA [threeLetters, threeLetters, threeLetters]) :: String -> Either String ([String], String)

-- any program is traversable - tokia minti sove
-- >>> runParser (sequenceA [threeLetters, threeLetters, threeLetters]) "asdasdasd!"
-- Right (["asd","asd","asd"],"!")

-- aprasau parseri, kuris turi Monad instance
-- Parser yra monadas, nes Parser yra Applicative ir Applicative yra Functor
-- monad negali egzistuoti be Applicative ir Functor
-- pvz functor gali papuosti sumustini (naudodamas fmap)
-- applicative gali paimti du sumustinius ir sujungti juos i nauja sumustini
-- monad gali paimti viena sumustini, atidaryti ji, pasiimti is jo reiksme ir su ja kazka daryti
instance Monad Parser where
    -- >>= bind operator for chaining monadic operations
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- vienas parseris ivykdo, paima rezultata ir paduoda kita parseriui
    -- priimam input, paleidziam pirma parseri
    -- jei pirmas parseris nepavyko, grazinam klaida
    -- jei pavyko, paimam rezultata ir likusi inputa
    -- ir paleidziam antra parseri su rezultatu is pirmo parserio
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) ->
                -- paduodam rezultata is pirmo parserio i mf
                -- ir remaining dali
                case runParser (mf a) r1 of
                    Left e2 -> Left e2
                    Right (b, r2) -> Right (b, r2)

-- >>> runParser threeThreeLetterWords "asdasdasd!"
-- Right (["asd","asd","asd"],"!")
threeThreeLetterWords :: Parser [String]
threeThreeLetterWords = do
    a <- threeLetters -- grazina String
    b <- threeLetters
    c <- threeLetters
    pure [a, b, c] -- grazina Parser [String]

-- >>> runState stateful "initial"
-- (7,"I am a new state")
-- State s a - s -> (a, s)
-- runState :: State s a -> s -> (a, s) 
-- tai yra laukas autmatiskai sukurtas newtype State s a = StateT s Identity a
stateful :: State String Int -- State monadas, kuri laiko String busena ir grazina Int
stateful = do
    value <- get -- paimamas esamas state'as pvz initial
    put "I am a new state" -- pakeiciam busena i tokia
    pure $ length value -- graziname ilgi seno state

-- >>> runState combined "initial"
-- ((7,16),"I am a new state")
combined :: State String (Int, Int)
combined = do
    a <- stateful -- grazina int
    b <- stateful -- grazina int 
    pure (a, b) -- grazina pora int'u liste????

-- >>> runState combined' "initial"
-- ((7,16),"I am a new state")
-- combined applicative version
-- <$> pritaiko stateful pirmai reiksmei
-- <$> infix version of fmap
-- <*> pritaiko stateful antrai reiksmei
-- <*> infix operator to apply functions in applicative context to values in applicative context
-- (,) sujungia i pora
combined' :: State String (Int, Int)
combined' = (,) <$>  stateful <*> stateful

-- >>> runState combined'' "initial"
-- ((7,16),"I am a new state")
combined'' :: State String (Int, Int)
combined'' = liftA2 (,) stateful stateful
