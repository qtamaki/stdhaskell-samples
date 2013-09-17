module Stack (Stack, emptyStack, push, pop, top, isEmpty) where

data Stack a = Stack [a]  deriving Show

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Stack a
pop (Stack [])     = error "pop emptyStack"
pop (Stack (_:xs)) = Stack xs

top :: Stack a -> a
top (Stack [])    = error "top emptyStack"
top (Stack (x:_)) = x

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs
