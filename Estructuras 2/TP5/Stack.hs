module Stack
(Stack,emptyS,isEmptyS,push,top,pop,lenS)
where

data Stack a = S [a] deriving Show

emptyS :: Stack a
isEmptyS :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> Stack a
lenS :: Stack a -> Int

stack1 = push 20 (push 10 emptyS)

emptyS = S []

isEmptyS (S xs) = null xs

push x (S []) = (S [x])
push x (S ys) = (S (x:ys))

top (S xs) = head xs

pop (S xs) = S (tail xs)

--lenS (S (x:xs)) = if null xs then 1 else 1 + lenS (S xs)
lenS (S []) = 0
lenS (S (x:xs)) = 1 + lenS (S xs)

