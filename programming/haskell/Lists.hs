module Lists where

import Prelude hiding(head, tail, last, init, length, null, reverse, take, drop)

head :: [a] -> a
head (x : _) = x

tail :: [a] -> [a]
tail (_ : xs) = xs

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs

init :: [a] -> [a]
init [] = []
init [_] = []
init (x : xs) = x : init xs

length :: [a] -> Int
length [] = 0
length (x : xs) = 1 + length xs

null :: [a] -> Bool
null [] = True
null _ = False

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

take :: Int -> [a] -> [a]
take 0 xs = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n (x : xs) = drop (n - 1) xs
