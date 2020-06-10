module Data.Split (
    Split,
    split,
    swap,
    left,
    right,
    total,
    normalize,
    moveLeft,
    moveRight,
    addLeft,
    addRight,
    removeLeft,
    removeRight
) where


newtype Split a = Split (a, a) deriving (Eq, Show)


split :: (Num a, Ord a) => a -> a -> Split a
split l r = normalize $ Split (l, r)

swap :: Split a -> Split a
swap (Split (l, r)) = Split (r, l)

left :: Split a -> a
left (Split (l, _)) = l

right :: Split a -> a
right (Split (_, r)) = r

total :: Num a => Split a -> a
total (Split (l, r)) = l + r

normalize :: (Num a, Ord a) => Split a -> Split a
normalize (Split (l, r))
    | l + r < 0 = Split (0, 0)
    | l < 0 = Split (0, r + l)
    | r < 0 = Split (l + r, 0)
    | otherwise = Split (l, r)

moveLeft :: (Num a, Ord a) => a -> Split a -> Split a
moveLeft a (Split (l, r)) = normalize $ Split (l + a, r - a)

moveRight :: (Num a, Ord a) => a -> Split a -> Split a
moveRight a (Split (l, r)) = normalize $ Split (l - a, r + a)

addLeft :: (Num a, Ord a) => a -> Split a -> Split a
addLeft a (Split (l, r)) = Split (l + a, r)

addRight :: (Num a, Ord a) => a -> Split a -> Split a
addRight a (Split (l, r)) = Split (l, r + a)

removeLeft :: (Num a, Ord a) => a -> Split a -> Split a
removeLeft a (Split (l, r)) = normalize $ Split (l - a, r)

removeRight :: (Num a, Ord a) => a -> Split a -> Split a
removeRight a (Split (l, r)) = normalize $ Split (l, r - a)

instance Functor Split where
    fmap f (Split (l, r)) = Split (f l, f r)
