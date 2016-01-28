{-# LANGUAGE RankNTypes #-}

-- Boolean
newtype Bool' = Bool' { runBool' :: forall a. a -> a -> a }

-- constructors
true' :: Bool'
true' = Bool' $ \t f -> t

false' :: Bool'
false' = Bool' $ \t f -> f

-- other
not' :: Bool' -> Bool'
not' a = runBool' a false' true'

and' :: Bool' -> Bool' -> Bool'
and' a b = runBool' a b false'

or' :: Bool' -> Bool' -> Bool'
or' a b = runBool' a true' b

xor' :: Bool' -> Bool' -> Bool'
xor' a b = runBool' a false' b

if' :: Bool' -> a -> a -> a
if' c t f = runBool' c t f


-- Natural numbers
newtype Nat' = Nat' { runNat' :: forall a. (a -> a) -> a -> a }

-- constructors
zero' :: Nat'
zero' = Nat' $ \s z -> z

succ' :: Nat' -> Nat'
succ' n = Nat' $ \s z -> s (runNat' n s z)

-- other
isZero' :: Nat' -> Bool'
isZero' n = runNat' n (\n -> false') true'

isEven' :: Nat' -> Bool'
isEven' n = runNat' n not' true'

add' :: Nat' -> Nat' -> Nat'
add' x y = Nat' $ \s z -> runNat' y s (runNat' x s z)

mul' :: Nat' -> Nat' -> Nat'
mul' x y = Nat' $ \s z -> runNat' y (runNat' x s) z

exp' :: Nat' -> Nat' -> Nat'
exp' b e = Nat' $ runNat' e (runNat' b)


type Pair' a b = forall c. (a -> b -> c) -> c

-- constructors
pair' :: a -> b -> Pair' a b
pair' a b = \p -> p a b

-- other
fst' :: Pair' a b -> a
fst' p = p (\a b -> a)

snd' :: Pair' a b -> b
snd' p = p (\a b -> b)


newtype List' a = List' { runList' :: forall b. (a -> b -> b) -> b -> b }

-- constructors
nil' :: List' a
nil' = List' $ \c n -> n

cons' :: a -> List' a -> List' a
cons' h t = List' $ \c n -> c h (runList' t c n)

-- other
null' :: List' a -> Bool'
null' l = runList' l (\h t -> false') true'

fold' :: (a -> b -> b) -> b -> List' a -> b
fold' f a xs = runList' xs f a

map' :: (a -> b) -> List' a -> List' b
map' f xs = fold' (\h t -> cons' (f h) t) nil' xs

filter' :: (a -> Bool') -> List' a -> List' a
filter' p xs = fold' (\x xs -> if' (p x) (cons' x (filter' p xs)) (filter' p xs)) nil' xs


newtype Maybe' a = Maybe' { runMaybe' :: forall b. (a -> b) -> b -> b }

-- constructors
nothing' :: Maybe' a
nothing' = Maybe' $ \j n -> n

just' :: a -> Maybe' a
just' a = Maybe' $ \j n -> j a

-- other
isNothing' :: Maybe' a -> Bool'
isNothing' m = runMaybe' m (\_ -> false') true'

isSomething' :: Maybe' a -> Bool'
isSomething' m = runMaybe' m (\_ -> true') false'

getSomething' :: Maybe' a -> a
getSomething' m = runMaybe' m id undefined


newtype Either' a b = Either' { runEither' :: forall c. (a -> c) -> (b -> c) -> c }

-- constructors
left' :: a -> Either' a b
left' a = Either' $ \l r -> l a

right' :: b -> Either' a b
right' b = Either' $ \l r -> r b

-- other
isLeft' :: Either' a b -> Bool'
isLeft' e = runEither' e (\_ -> true') (\_ -> false')

isRight' :: Either' a b -> Bool'
isRight' e = runEither' e (\_ -> false') (\_ -> true')

getLeft' :: Either' a b -> a
getLeft' e = runEither' e id undefined

getRight' :: Either' a b -> b
getRight' e = runEither' e undefined id