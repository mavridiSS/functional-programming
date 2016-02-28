saySign x
  | x > 0 = "Positive"
  | x == 0 = "Zero"
  | x < 0 = "Negative"

useless 0 _ _ _ = 1
useless _ 0 _ _ = 1
useless _ _ 0 _ = 1
useless _ _ _ 0 = 1
useless a b c d = a + b + c + d

useless' a b c d = if a==0 || b==0 || c==0 || d==0
                      then 1
                      else a + b + c + d

maxThree a b c
  | a < b = if b < c then c else b
  | otherwise = if a < c then c else a


fact1 0 = 1
fact1 n = n * fact1 (n - 1)

fact2 n = if n == 0 then 1 else n * fact2 (n - 1)

fact3 n
  | n == 0 = 0
  | otherwise = n * fact3 (n - 1)

fact4 n = case n of 0 -> 0
                    n -> n * fact4 (n - 1)


fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n - 1) + fib2 (n - 2)

fib2 n = if n < 2 then n else fib2 (n - 1) + fib2 (n - 2)

fib3 n
  | n < 2 = n
  | otherwise = fib3 (n - 1) + fib3 (n - 2)

fib4 n = case n of 0 -> 0
                   1 -> 1
                   n -> fib4 (n - 1) + fib4 (n - 2)

fib5 n = if n < 0 then -1 else helper 0 1 0
  where helper a b idx
          | idx == n = a
          | otherwise = helper b (a + b) (succ idx)

discr a b c = b^2 - 4*a*c

countRoots a b c
  | a == 0 = "Linear equation!"
  | d < 0 = "No roots!"
  | d == 0 = "One root!"
  | d > 0 = "Two roots!"
  where d = discr a b c

sayRoots a b c
  | a == 0 = "Linear equation!"
  | d < 0 = "No roots!"
  | proizv < 0 = "Positive and negative!"
  | proizv == 0 && sbor < 0 = "Negative and zero!"
  | proizv == 0 && sbor == 0 = "Both zero!"
  | proizv == 0 && sbor > 0 = "Positive and zero!"
  | sbor < 0 = "Both negative!"
  | sbor > 0 = "Both positive!"
  where d = discr a b c
        proizv = c/a
        sbor = (- b/a)

cylinderVolume r h = lidArea * h
  where lidArea = pi * r * r

