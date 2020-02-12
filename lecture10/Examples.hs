{-# OPTIONS_GHC -Wall #-}

module Examples where

import Prelude hiding (map, take, drop)

--
-- List examples
--

-- map ::
map = undefined

-- append ::
append  = undefined

-- take ::
take = undefined

-- drop ::
drop = undefined

-- Define a List data type that represents lists that can contain any type of
-- element.
data List a = Nil
          | Cons a(List)

-- data List

--
-- Partial and total functions
--

-- Define a NonEmptyList data type and associated functions
-- data NonEmptyList
data NonEmptyList a = NEL a  [a]

-- nelToList ::
nelToList :: NonEmptyList a -> [a]
nelToList(NEL x xs)= : xs

-- listToNel ::
listToNel :: [a] -> NonEmptyList a
listToNel []  = error "Empty list"
listToNel (x:xs) = NEL x xs

-- headNEL ::
headNEL (NEL x _) = x


-- tailNEL ::
tailNEL (NEL _ xs) = xs

--
-- Higher-order functions
--

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- prefix notation for dots
(.) :: (b -> c) -> (a -> b) -> a -> c
--- Takes an argument type b->c and argument type a->b and gives us back a->c
--- syntax for lambda = \x
--- x must have type a
--- return a, run g on it to get type b. Then run f on it to get type c

--  f . g = \x -> f g a
(.) f g x = f (g x)

($) :: (a -> b) -> a -> b
-- All I can do is apply f to x
f $ x = f x

--- takes argument all at once, and give back function that takes argument one at a time
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y
uncurry f = \(x, y) -> f x y


-- What about papply?


--
-- Natural numbers
--

data Nat = Zero | Succ Nat

-- isNat takes a Natural number and tells me if it's natural
isNat :: Nat -> Bool
isNat _ = True

--- Converts a natural number to an int
nat2int :: Nat -> Int
nat2int = error "nat2int: not yet defined"
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat = error "int2nat: not yet defined"
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- This is adding natural numbers
addCheat :: Nat -> Nat -> Nat
addCheat m n = int2nat (nat2int m + nat2int n)

add :: Nat -> Nat -> Nat
-- add = error "add: not yet defined"
add Zero n = n
add (Succ m) n = Succ (add m n)

--
-- Arithmetic expression
--

-- Expression is either a Value, which is integer,
-- or it could be Add of two expression or multiple of two expression
data Exp = Val Int
         | Add Exp Exp
         | Mul Exp Exp

-- The representation of the expression tree from lecture
e :: Exp
e = Add (Val 1) (Mul (Val 2) (Val 3))

-- Return the "size" of the expression, which is the number of constants it
-- contains.
size :: Exp -> Int
size = error "size: not yet defined"

-- Evaluate an expression
eval :: Exp -> Int
-- eval = error "eval: not yet defined"
eval (Val x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

--
-- Binary trees
--

-- A tree could either contain a leaf, or it could contain a node that consist of a tree, a value, or a tree
-- Define a data type for binary trees
data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

-- The representation of the tree from lecture
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4))
          5
          (Node (Leaf 6) 7 (Leaf 9))

-- Decide if a given value occurs in a binary tree
-- occurs = error "occurs: not yet defined"
occurs :: a -> Tree a -> Bool
occurs x = (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x 1 || occurs x rs

-- Flatten a tree into a list of its values
flatten = error "flatten: not yet defined"

-- Search for a value in an *ordered* tree (values in left subtree are <, values
-- in right subtree are >).
search = error "search: not yet defined"

--
-- Type classes
--

data Foo = F Int | G Char

-- instance Eq Foo where
