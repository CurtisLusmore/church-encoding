{-# LANGUAGE Rank2Types #-}

-- Boolean
type Bool' = forall a. a -> a -> a

-- constructors
true' :: Bool'
true' = \t f -> t

false' :: Bool'
false' = \t f -> f

-- other
not' :: Bool' -> Bool'
not' a = a false' true'

and' :: Bool' -> Bool' -> Bool'
and' a b = a b false'

or' :: Bool' -> Bool' -> Bool'
or' a b = a true' b

xor' :: Bool' -> Bool' -> Bool'
xor' a b = a false' b

if' :: Bool' -> a -> a -> a
if' c t f = c t f


-- Natural numbers
type Nat' = forall a. (a -> a) -> a -> a

-- constructors
zero' :: Nat'
zero' = \s z -> z

succ' :: Nat' -> Nat'
succ' n = \s z -> s (n s z)

-- other
isZero' :: Nat' -> Bool'
isZero' n = n (\n -> false') true'


type Pair' a b = forall c. (a -> b -> c) -> c

-- constructors
pair' :: a -> b -> Pair' a b
pair' a b = \p -> p a b

-- other
fst' :: Pair' a b -> a
fst' p = p (\a b -> a)

snd' :: Pair' a b -> b
snd' p = p (\a b -> b)


type List' a = forall b. (a -> b -> b) -> b -> b

-- constructors
nil' :: List' a
nil' = \c n -> n

cons' :: a -> List' a -> List' a
cons' h t = \c n -> c h (t c n)

-- other
null' :: List' a -> Bool'
null' l = l (\h t -> false') true'

fold' :: (a -> a -> a) -> a -> List' a -> a
fold' f a as = as f a


type Maybe' a = forall b. b -> (a -> b) -> b

-- constructors
nothing' :: Maybe' a
nothing' = \n j -> n

just' :: a -> Maybe' a
just' a = \n j -> j a


type Either' a b = forall c. (a -> c) -> (b -> c) -> c

-- constructors
left' :: a -> Either' a b
left' a = \l r -> l a

right' :: b -> Either' a b
right' b = \l r -> r b