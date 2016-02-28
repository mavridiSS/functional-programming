count :: (Eq a) => a -> [a] -> Int
count x l = length [ k | k<-l, k==x]

count' :: (Eq a) => a -> [a] -> Int
count' x l = length $ filter (==x) l

sumProducts :: (Num a) => [[a]] -> a
sumProducts ls = sum $ map product ls

occurrences :: (Eq a) => [a] -> [a] -> [Int]
occurrences lst1 lst2 = map (\x -> count x lst2) lst1

occurrences1 :: (Eq a) => [a] -> [a] -> [Int]
occurrences1 lst1 lst2 = map (`count` lst2) lst1

data Tree = Empty | Node Int Tree Tree

insertBST :: Int -> Tree -> Tree
insertBST x Empty = Node x Empty Empty
insertBST x (Node root left right)
  | root  < x = Node root left (insertBST x right)
  | root == x = Node root left right
  | root  > x = Node root (insertBST x left) right

-- data Maybe a = Nothing | Just a

maybePlus :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybePlus Nothing _ = Nothing
maybePlus _ Nothing = Nothing
maybePlus (Just x) (Just y) = Just (x+y)

-- po uslovie rabotim za matrica s chisla, zaradi koeto
-- postavqme ogranichenieto (Num a) - inache e nenujno.
isSquareMatrix :: (Num a) => [[a]] -> Bool
isSquareMatrix m = all (== length m) $ map length m

mainDiag :: (Num a) => [[a]] -> [a]
mainDiag m = map (\idx -> (m !! idx) !! idx) [0..(length m) - 1]

mainDiag' :: (Num a) => [[a]] -> [a]
mainDiag' m = zipWith (!!) m [0..]

sndDiag :: (Num a) => [[a]] -> [a]
sndDiag m = mainDiag $ map reverse m

composeN :: (a -> a) -> Int -> (a -> a)
composeN f 0 = (\x -> x)
composeN f n = f . composeN f (n-1)

composeList :: (a -> a) -> [(a -> a)]
composeList f = map (\x -> composeN f x) [1..]

matchLengths :: (Eq a) => [[a]] -> Bool
matchLengths ls = let lengths = map length ls
                    in all (== head lengths) lengths