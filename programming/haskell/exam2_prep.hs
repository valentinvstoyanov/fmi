fldr :: (a -> b -> b) -> b -> [a] -> b
fldr _ nv []      = nv
fldr op nv (x:xs) = x `op` fldr op nv xs 

fldl :: (b -> a -> b) -> b -> [a] -> b
fldl _ nv []      = nv
fldl op nv (x:xs) = fldl op (op nv x) xs

fldr1 :: (a -> a -> a) -> [a] -> a
fldr1 _ [x]     = x
fldr1 op (x:xs) = x `op` fldr1 op xs

fldl1 :: (a -> a -> a) -> [a] -> a
fldl1 _ [x]       = x
fldl1 op (x:y:xs) = fldl1 op (op x y : xs)

mp :: (a -> b) -> [a] -> [b]
mp f = fldr ((:).f) []

filtr :: (a -> Bool) -> [a] -> [a]
filtr p = fldr (\x r -> if p x then x:r else r) []

iter :: (a -> a) -> a -> [a]
iter f x = x : iter f (f x)

scnl :: (b -> a -> b) -> b -> [a] -> [b]
scnl op nv = foldl (\acc x -> acc ++ [(last acc) `op` x]) [nv]

scnr :: (a -> b -> b) -> b -> [a] -> [b]
scnr op nv = foldr (\x acc -> x `op` (head acc) :  acc) [nv] 

dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

rep :: a -> Int -> [a]
rep x n = [ x | _ <- [1..n]]

repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = (rep x n) ++ (repli xs n)

dropEvery :: [a] -> Int -> [a]
dropEvery l n = loop l n n
  where loop :: [a] -> Int -> Int -> [a]
        loop [] _ _     = []
        loop (x:xs) n 1 = loop xs n n
        loop (x:xs) n c = x : loop xs n (c - 1) 

split :: [a] -> Int -> ([a], [a])
split [] _     = ([], [])
split xs 0     = ([], xs)
split (x:xs) n = (x:ys, zs)
  where (ys,zs) = split xs (n - 1)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _  = []
slice (x:xs) i k
  | i > k     = []
  | i == 1    = x : slice xs 1 (k - 1)
  | otherwise = slice xs (i - 1) (k - 1) 

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate l@(x:xs) n
  | n > 0   = rotate (xs ++ [x]) (n - 1) 
  | n < 0   = rotate l (length xs + n + 1)

rmAt :: Int -> [a] -> [a]
rmAt _ []     = []
rmAt 1 (x:xs) = xs
rmAt n (x:xs) = x : rmAt (n - 1) xs

insAt :: a -> [a] -> Int -> [a]
insAt x [] _     = [x]
insAt x xs 1     = x:xs 
insAt x (y:ys) n = y:(insAt x ys (n - 1))

range :: Int -> Int -> [Int]
range i k
  | i > k     = []
  | otherwise = i:range (i + 1) k
