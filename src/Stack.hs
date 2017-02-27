
module Stack where

type Stack a = [a]

create :: Stack a
create = []

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> (a, Stack a)
pop [] = error "Stack is empty"
pop (x:xs) = (x, xs)

peek :: Stack a -> a
peek [] = error "Stack is empty"
peek (x:_) = x

empty :: Stack a -> Bool
empty [] = True
empty (_:_)= False

