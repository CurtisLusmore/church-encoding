{-# LANGUAGE RankNTypes #-}

-- helpers
id' :: a -> a
id' = \x -> x

const' :: a -> b -> a
const' = \x _ -> x

const2' :: a -> b -> c -> a
const2' = \x _ _ -> x

undefined' :: a
undefined' = undefined'


-- Boolean
newtype Bool' = Bool' { runBool' :: forall a. a -> a -> a }

-- constructors
true' :: Bool'
true' = Bool' $ \true false -> true

false' :: Bool'
false' = Bool' $ \true false -> false

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
zero' = Nat' $ \succ zero -> zero

succ' :: Nat' -> Nat'
succ' n = Nat' $ \succ zero -> succ (runNat' n succ zero)

-- other
isZero' :: Nat' -> Bool'
isZero' n = runNat' n (const' false') true'

isEven' :: Nat' -> Bool'
isEven' n = runNat' n not' true'

add' :: Nat' -> Nat' -> Nat'
add' x y = Nat' $ \succ zero -> runNat' y succ (runNat' x succ zero)

mul' :: Nat' -> Nat' -> Nat'
mul' x y = Nat' $ \succ zero -> runNat' y (runNat' x succ) zero

exp' :: Nat' -> Nat' -> Nat'
exp' b e = Nat' $ runNat' e (runNat' b)


-- Pair
type Pair' a b = forall c. (a -> b -> c) -> c

-- constructors
pair' :: a -> b -> Pair' a b
pair' a b = \pair -> pair a b

-- other
fst' :: Pair' a b -> a
fst' p = p $ \fst snd -> fst

snd' :: Pair' a b -> b
snd' p = p $ \fst snd -> snd


-- List
newtype List' a = List' { runList' :: forall b. (a -> b -> b) -> b -> b }

-- constructors
nil' :: List' a
nil' = List' $ \cons nil -> nil

cons' :: a -> List' a -> List' a
cons' x xs = List' $ \cons nil -> cons x (runList' xs cons nil)

-- other
null' :: List' a -> Bool'
null' l = runList' l (const2' false') true'

fold' :: (a -> b -> b) -> b -> List' a -> b
fold' f a xs = runList' xs f a

map' :: (a -> b) -> List' a -> List' b
map' f xs = fold' (\x xs -> cons' (f x) xs) nil' xs

filter' :: (a -> Bool') -> List' a -> List' a
filter' p xs = fold' (\x xs -> if' (p x) (cons' x xs) xs) nil' xs


-- Maybe
newtype Maybe' a = Maybe' { runMaybe' :: forall b. (a -> b) -> b -> b }

-- constructors
nothing' :: Maybe' a
nothing' = Maybe' $ \just nothing -> nothing

just' :: a -> Maybe' a
just' a = Maybe' $ \just nothing -> just a

-- other
isNothing' :: Maybe' a -> Bool'
isNothing' m = runMaybe' m (const' false') true'

isSomething' :: Maybe' a -> Bool'
isSomething' m = runMaybe' m (const' true') false'

getSomething' :: Maybe' a -> a
getSomething' m = runMaybe' m id' undefined'


-- Either
newtype Either' a b = Either' { runEither' :: forall c. (a -> c) -> (b -> c) -> c }

-- constructors
left' :: a -> Either' a b
left' a = Either' $ \left right -> left a

right' :: b -> Either' a b
right' b = Either' $ \left right -> right b

-- other
isLeft' :: Either' a b -> Bool'
isLeft' e = runEither' e (const' true') (const' false')

isRight' :: Either' a b -> Bool'
isRight' e = runEither' e (const' false') (const' true')

getLeft' :: Either' a b -> a
getLeft' e = runEither' e id' undefined'

getRight' :: Either' a b -> b
getRight' e = runEither' e undefined' id'