-- | Notes taken by Emilija Rimšelytė
module Lessons.Lesson07 where
import Data.Function (on)

-- | List comprehension: pairing numbers with letters.
-- For each 'a' and 'b', we build a tuple.
-- A cartesian product, clean and subtle.
--
-- >>> lc 
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b')]
-- | In parentheses we show the expected output of the expression
-- '|' - means for each
lc :: [(Integer, Char)]
lc = [(a, b) | a <- [1,2,3,4], b <- ['a', 'b']]

-- | Squares of 'a', repeated for each 'b'.
-- 'b' isn’t used, but it multiplies the count.
-- More combinations — that’s the amount.
--
-- >>> lc'
-- [1,1,4,4,9,9]
-- | b is never used, but still generates combinations
lc' :: [Integer]
lc' = [a * a | a <- [1,2,3], b <- [1,2]]

-- | 'b' repeats for each 'c', though 'c' is unused.
-- 'a' is fixed, but nesting boosts the size.
--
-- >>> lc''
-- [1,1,2,2,3,3,4,4]
-- | outputs only b
lc'' :: [Integer]
lc'' = [b | a <- [1], b <- [1,2,3,4], c <- ['a', 'z']]

-- | Empty list in 'b' means no results.
-- Comprehension short-circuits — nothing to compute.
--
-- >>> lc'''
-- []
-- | because one generator produces no values, the result is empty
lc''' :: [Integer]
lc''' = [a | a <- [1,2,3], b <- []]

-- | Same idea, but now 'a' is empty.
-- No values to pair, so the list stays bare.
--
-- >>> lc''''
-- []
lc'''' :: [Integer]
lc'''' = [a | a <- [], b <- [1,2,3]]

-- | Do-notation version of 'lc'.
-- Same result, just written differently.
-- Shows how monads can express list logic.
--
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

-- | Swapping the order changes the output.
-- First 'b', then 'a' — so the nesting flips.
--
-- >>> lm'
-- [(1,'a'),(2,'a'),(3,'a'),(4,'a'),(1,'b'),(2,'b'),(3,'b'),(4,'b')]

lm' :: [(Integer, Char)]
lm' = do
    b <- ['a', 'b']
    a <- [1,2,3,4]
    return (a, b)

-- | Maybe monad: chaining computations.
-- All values must be 'Just', or the chain breaks.
-- One 'Nothing', and the whole thing shakes.
--
-- >>> mm
-- Just 84
-- multiply values inside Just if they both exist
mm :: Maybe Integer
mm = do
    a <- Just 42 -- extract value from Just
    b <- Just 2
    return $ a * b -- wrap the result back into Just

-- | One 'Nothing' in the chain — result is 'Nothing'.
-- That’s how Maybe guards against failure.
--
-- >>> mm'
-- Nothing
-- in monad, where one of the values is Nothing, the whole result is Nothing
mm' :: Maybe Integer
mm' = do
    a <- Just 42
    b <- Just 2
    c <- Nothing
    return $ a * b

-- | Looks like it should work, but it doesn’t.
-- You need to bind the final value too.
-- Just expression alone won’t pull it through.
--
-- >>> mm''
-- Nothing
-- grazintu Just 84
-- Just can be used instead of return here
mm'' :: Maybe Integer
mm'' = do
    a <- Just 42
    b <- Just 2
    Just $ a * b

-- | Either monad: 'Right' flows like Maybe’s 'Just'.
-- 'Left' short-circuits — it’s an error path.
--
-- >>> em
-- Right 'a'
em :: Either String Char
em = do
    a <- Right 43 -- a = 43
    b <- Right 'a' -- b = 'a'
    return b

-- | 'Left' stops the computation early.
-- Doesn’t matter what comes after — it’s done.
--
-- >>> em'
-- Left "oh"
-- either monad has Right and Left values
em' :: Either String Char
em' = do
    a <- Left "oh"
    b <- Right 'a'
    return b

-- | You can return tuples too — Either handles it.
-- As long as all are 'Right', it’s alright.
--
-- >>> em''
-- Right (43,'a')
em'' :: Either String (Integer, Char)
em'' = do
    a <- Right 43
    b <- Right 'a'
    return (a, b)

-- | Mixing 'Left' and 'Right' — 'Left' wins.
-- Error takes over, no multiplication begins.
--
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

-- mm = do
--     a <- Just 42
--     b <- Just 2
--     return $ a * b

-- | Same as 'lm', but using '>>=' explicitly.
-- Do-notation is just syntactic sugar.
-- Shows the plumbing under the rug.
lmb :: [(Integer, Char)]
lmb = [1,2,3,4] >>= (\a -> ['a', 'b'] >>= (\b -> return (a, b)))

-- | Same as 'mm', but written with '>>='.
-- Shows how monads chain computations.
--
-- >>> mmb
-- Just 84
mmb :: Maybe Integer
mmb = Just 42 >>= (\a -> Just 2 >>= (\b -> return $ a * b))

-- >>= so we can continue the chain 
-- em = do
--     a <- Right 43
--     b <- Right 'a'
--     return b

-- | Same as 'em', but with explicit binds.
-- Monads let you sequence effects — even errors.
emb :: Either String Char
emb = Right 43 >>= (\a -> Right 'a' >>= (\b -> return b))