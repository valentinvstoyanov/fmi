group :: Eq a => [a] -> [[a]]
group []     = []
group (x:xs) = (x : prefix) : group suffix
    where (prefix, suffix) = span (x ==) xs

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ []     = []
sortBy f (x:xs) = sortBy f lt ++ et ++ sortBy f gt
    where lt = [y | y <- xs, f y x == LT]
          gt = [y | y <- xs, f y x == GT]
          et = x : [y | y <- xs, f y x == EQ]

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ []     = []
groupBy p (x:xs) = (x : prefix) : groupBy p suffix 
    where (prefix, suffix) = span (p x) xs

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a1 a2 = g a1 `f` g a2

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g a = (f a, g a)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = sortBy (compare `on` f) xs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f xs = groupBy ((==) `on` f) xs

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f xs = groupOn f (sortOn f xs)

data NonEmpty a = a :| [a] deriving Show

headNonEmpty :: NonEmpty a -> a
headNonEmpty (x :| _) = x 

consNonEmpty :: a -> NonEmpty a -> NonEmpty a 
consNonEmpty x (y :| ys) = x :| (y:ys)

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty []  = []
groupNonEmpty [x] = [x :| []]
groupNonEmpty (x:xs)
    | x == headNonEmpty hg = (consNonEmpty x hg) : tg
    | otherwise    = (x :| []) : g 
    where g@(hg:tg) = groupNonEmpty xs

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty f [] = []
groupByNonEmpty f [x] = [x :| []]
groupByNonEmpty f (x:xs)
    | x `f` (headNonEmpty hg) = (consNonEmpty x hg) : tg
    | otherwise               = (x :| []) : g 
    where g@(hg:tg) = groupByNonEmpty f xs

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f xs = groupByNonEmpty ((==) `on` f) xs 

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f xs = groupOnNonEmpty f (sortOn f xs)
