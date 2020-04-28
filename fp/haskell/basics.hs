x :: Int
x = 5

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

lastDigit :: Int -> Int
lastDigit = (`mod` 10)
