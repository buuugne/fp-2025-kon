module Lessons.Lesson07 where
import Data.Function (on)

-- >>> lc 
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b')]
-- | In parentheses we show the expected output of the expression
-- '|' - means for each
lc :: [(Integer, Char)]
lc = [(a, b) | a <- [1,2,3,4], b <- ['a', 'b']]

-- >>> lc'
-- [1,1,4,4,9,9]
-- | b is never used, but still generates combinations
lc' :: [Integer]
lc' = [a * a | a <-[1,2,3], b <- [1,2] ]

-- >>> lc''
-- [1,1,2,2,3,3,4,4]
-- | outputs only b
lc'' :: [Integer]
lc'' = [b | a <- [1], b <- [1,2,3,4], c <- ['a', 'z']]

-- >>> lc'''
-- []
-- | because one generator produces no values, the result is empty
lc''' :: [Integer]
lc''' = [a| a <- [1,2,3], b <- []]

-- >>> lc''''
-- []
lc'''' :: [Integer]
lc'''' = [a| a <- [], b <- [1,2,3]]

-- >>> lm
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b')] 

-- list type is a monad. why? what is monad?
-- monad - design pattern for chaining computations together
-- Maybe, Either, [], IO
--
-- here we use do notation to express the same as in list comprehension above
lm :: [(Integer, Char)]
lm = do
    a <- [1,2,3,4]
    b <- ['a', 'b']
    return (a, b)

-- >>> lm'
-- [(1,'a'),(2,'a'),(3,'a'),(4,'a'),(1,'b'),(2,'b'),(3,'b'),(4,'b')]

lm' :: [(Integer, Char)]
lm' = do
    b <- ['a', 'b']
    a <- [1,2,3,4]
    return (a, b)

-- >>> mm
-- Just 84
-- multiply values inside Just if they both exist
mm :: Maybe Integer
mm = do
    a <- Just 42 -- extract value from Just
    b <- Just 2
    return $ a * b -- wrap the result back into Just

-- >>> mm'
-- Nothing
-- in monad, where one of the values is Nothing, the whole result is Nothing
mm' :: Maybe Integer
mm' = do
    a <- Just 42
    b <- Just 2
    c <- Nothing
    return $ a * b

-- >>> mm''
-- Nothing
-- grazintu Just 84
-- Just can be used instead of return here
mm'' :: Maybe Integer
mm'' = do
    a <- Just 42
    b <- Just 2
    Just $ a * b

-- >>> em
-- Right 'a'
em :: Either String Char
em = do
    a <- Right 43 -- a = 43
    b <- Right 'a' -- b = 'a'
    return b

-- >>> em'
-- Left "oh"
-- either monad has Right and Left values
em' :: Either String Char
em' = do
    a <- Left "oh"
    b <- Right 'a'
    return b

-- result - Right (43,'a')
em'' :: Either String (Integer, Char)
em'' = do
    a <- Right 43
    b <- Right 'a'
    return (a, b)

-- >>> em'''
-- Left 42
em''' :: Either Integer Integer
em''' = do
    a <- Right 43
    b <- Left 42
    return $ a * b

-- lm = do
--     a <- [1,2,3,4]
--     b <- ['a', 'b']
--     return (a, b)
-- without do notation
-- taking a from [1,2,3,4]
-- for each a, taking b from ['a','b']
-- >>= is bind operator, which takes a value from the monad and a function that returns a value in the same monad
lmb :: [(Integer, Char)]
lmb = [1,2,3,4] >>= (\a -> ['a', 'b'] >>= (\b -> return (a, b)))

-- mm = do
--     a <- Just 42
--     b <- Just 2
--     return $ a * b
-- >>> mmb
-- Just 84
mmb :: Maybe Integer
mmb = Just 42 >>= (\a -> Just 2 >>= (\b -> return $ a * b))

-- >>= so we can continue the chain 
-- em = do
--     a <- Right 43
--     b <- Right 'a'
--     return b
emb :: Either String Char
emb = Right 43 >>= (\a -> Right 'a' >>= (\b -> return b))