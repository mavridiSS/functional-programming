replicate' n x = take n (repeat x)

replicate2 n x
  | n <= 0 = []
  | otherwise = x : replicate2 (n - 1) x

take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x : take' (n - 1) xs

prime 1 = False
prime n = null [ k | k<-[2..sqn], n `mod` k == 0]
  where sqn = floor (sqrt (fromIntegral n))

primes = filter prime [1..]

nthPrime n = primes !! (n - 1)

first  (a,_,_) = a
second (_,b,_) = b
third  (_,_,c) = c

descartes lst1 lst2 = [ (x,y) | x<-lst1, y<-lst2 ]

pyths = [ (a,b,c) | a<-[1..100], b<-[1..100], c<-[1..100],
                    a^2 + b^2 == c^2, a + b + c <= 100]

quicksort [] = []
quicksort lst@(pivot:_) = quicksort [ x | x<-lst, x < pivot ]
                                 ++ [ x | x<-lst, x == pivot]
                       ++ quicksort [ x | x<-lst, x > pivot ]

stupidLength [] = "Empty list!"
stupidLength (_:[]) = "One element!"
stupidLength (_:_:[]) = "Two elements!"
stupidLength (_:_:_) = "More then two elements!"