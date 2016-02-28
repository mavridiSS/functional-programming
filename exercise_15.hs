-- Тъй като в зад.1 се изисква да поддържаме множествата сортирани,
-- е удобно да си дефинираме функция за сортиране. Най-лесно
-- на функционалните езици се дефинира quicksort. Нещо повече, можем
-- да си дефинираме функция за сортиране по подаден предикат за наредба
-- по подразбиране това е (<).

quickSort :: (Eq a, Ord a) => [a] -> [a]
quickSort [] = []
quickSort l@(pivot:_) = quickSort [ x | x<-l, x  < pivot] ++
                                  [ x | x<-l, x == pivot] ++
                        quickSort [ x | x<-l, x  > pivot]

quickSort' :: (a -> a -> Bool) -> [a] -> [a]
quickSort' _ [] = []
quickSort' comp l@(pivot:_) = quickSort' comp [ x | x<-l, comp x pivot] ++
                                              [ x | x<-l, not (comp x pivot || comp pivot x)] ++
                              quickSort' comp [ x | x<-l, comp pivot x]
-- тогава можем да дефинираме quickSort = quickSort' (<)

-- "стандартни" функции, които ще ни помогнат в повече от една задача
count :: (Eq a) => a -> [a] -> Int
count x l = length $ filter (==x) l

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll x l = filter (/=x) l

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (removeAll x xs)

histogram :: (Eq a) => [a] -> [(a,Int)]
histogram l = map (\x -> (x,count x l)) $ removeDuplicates l

---------------------------------------------------------------------
--   Зад.1

-- по дефиниция - AUB = AU(B\A). Това ни гарантира уникалност
-- на елементите, но после трябва да ги сортираме
setUnion' :: (Eq a, Ord a) => [a] -> [a] -> [a]
setUnion' xs ys = quickSort (xs ++ setDiff' ys xs)

-- отново по дефиниция - всички елементи от едното множество, които
-- са елементи на другото. Тук list comprehension ни запазва наредбата.
setIntersect' :: (Eq a, Ord a) => [a] -> [a] -> [a]
setIntersect' xs ys = [ x | x<-xs, x `elem` ys]

-- отново по дефиниция, и отново нямаме нужда от повторно сортиране
setDiff' :: (Eq a, Ord a) => [a] -> [a] -> [a]
setDiff' xs ys = [ x | x<-xs, not (x `elem` ys) ]

-- можем да използваме и вече дефинираната функция за обединение,
-- възползвайки се от правилото A/\B = (A\B)U(B\A)
setSymDiff' :: (Eq a, Ord a) => [a] -> [a] -> [a]
setSymDiff' xs ys = setUnion' (setDiff' xs ys) (setDiff' ys xs)

-- Другият подход към тези задачи е да "обхождаме"
-- успоредно двата списъка така, както ги "обхожда"
-- процедурата merge и на всяка стъпка да сравняваме
-- техните глави. За различните операции за множества
-- ще трябва да предприемаме различни действия, както и
-- когато някое от множествата е празно.

-- обединение: ако двете глави са равни, вкарваме само едната
-- в резултата, в противен случай добавяме по-малката
-- и така запазваме сортировката.
setUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
setUnion [] b = b
setUnion a [] = a
setUnion (x:xs) (y:ys)
  | x < y  = x : setUnion xs (y:ys)
  | x == y = x : setUnion xs ys
  | x > y  = y : setUnion (x:xs) ys

-- сечение: ако двете глави са равни, вкарваме само едната
-- в резултата, в противен случай не добавяме нищо.
setIntersect :: (Eq a, Ord a) => [a] -> [a] -> [a]
setIntersect [] b = []
setIntersect a [] = []
setIntersect (x:xs) (y:ys)
  | x < y  = setIntersect xs (y:ys)
  | x == y = x : setIntersect xs ys
  | x > y  = setIntersect (x:xs) ys

setDiff :: (Eq a, Ord a) => [a] -> [a] -> [a]
setDiff a [] = a
setDiff [] b = []
setDiff (x:xs) (y:ys)
  | x < y  = x : setDiff xs (y:ys)
  | x == y = setDiff xs ys
  | x > y  = setDiff (x:xs) ys

-- може и по същия метод, но това е тривиалната дефиниция
setSymDiff :: (Eq a, Ord a) => [a] -> [a] -> [a]
setSymDiff a b = setUnion (setDiff a b) (setDiff b a)
-- или setSymDiff a b = setDiff (setUnion a b) (setIntersect a b)

-- оказва се, че обединението на мултимножества е аналогично
multisetUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
multisetUnion a [] = a
multisetUnion [] b = b
multisetUnion (x:xs) (y:ys)
  | x < y  = x : multisetUnion xs (y:ys)
  | x == y = x : multisetUnion xs ys
  | x > y  = y : multisetUnion (x:xs) ys

-- както и сечението
multisetIntersect :: (Eq a, Ord a) => [a] -> [a] -> [a]
multisetIntersect a [] = []
multisetIntersect [] b = []
multisetIntersect (x:xs) (y:ys)
  | x < y  = multisetIntersect xs (y:ys)
  | x == y = x : multisetIntersect xs ys
  | x > y  = multisetIntersect (x:xs) ys

-- а пък сумата е класически merge
multisetSum :: (Eq a, Ord a) => [a] -> [a] -> [a]
multisetSum a [] = a
multisetSum [] b = b
multisetSum (x:xs) (y:ys)
  | x < y  = x : multisetSum xs (y:ys)
  | x == y = x : multisetSum xs (y:ys) -- за пълнота
  | x > y  = y : multisetSum (x:xs) ys

-- един вариант - да вземем всички уникални елементи на двете множества,
-- след което за всеки да преброим колко пъти трябва да го включим в
-- обединението. Повтарянето може да се направи с replicate, докато
-- concat слива вътрешните списъци. Този подход решава цялата задача
-- с три напълно аналогични функции.
multisetUnion' :: (Eq a, Ord a) => [a] -> [a] -> [a]
multisetUnion' xs ys = concat [replicate (f el) el | el<-uniques]
  where f x = max (count x xs) (count x ys)
        uniques = removeDuplicates (xs ++ ys)

multisetIntersect' :: (Eq a, Ord a) => [a] -> [a] -> [a]
multisetIntersect' xs ys = concat [replicate (f el) el | el<-uniques]
  where f x = min (count x xs) (count x ys)
        uniques = removeDuplicates (xs ++ ys)

multisetSum' :: (Eq a, Ord a) => [a] -> [a] -> [a]
multisetSum' xs ys = concat [replicate (f el) el | el<-uniques]
  where f x = (count x xs) + (count x ys)
        uniques = removeDuplicates (xs ++ ys)
-- или директно multisetSum' xs ys = quickSort (xs ++ ys)

---------------------------------------------------------------------------
--   Зад.2

-- ако имаме custom функция за сортиране,
-- можем да следваме директно условието - на всеки елемент
-- съпоставяме честотата на дължината му, т.е. колко са
-- другите елементи със същата дължина
lfsort :: [[a]] -> [[a]]
lfsort l = quickSort' (\x y -> freq x < freq y) l
  where freq x = length [ y | y<-l, length x == length y]

-- другият вариант е отново всеки списък да закачим в наредена двойка
-- със своята честота, които наредени двойки да сортираме по нормалния начин.
-- След това "отрязваме" числата, за да получим само оригиналните списъци.
lfsort' :: Ord a => [[a]] -> [[a]]
lfsort' l = map snd $ quickSort [ (sameLength x l, x) | x<-l ]
  where sameLength x l = length [ y | y<-l, length x == length y]

-- полезна функция за обръщане на наредени двойки
pairFlip :: (a,b) -> (b,a)
pairFlip (x,y) = (y,x)

-- отново можем да използваме директно решение със специалната функция за сортиране,
-- можем и да я решим постъпково. Основната идея и в двата случая е да съпоставим на
-- всеки вътрешен списък неговия най-често срещан елемент (което си е отделна задача
-- и може да се направи със друго сортиране), след което да сортираме по тези елементи.
hlsort :: (Eq a, Ord a) => [[a]] -> [[a]]
hlsort l = quickSort' (\x y -> mostFreq x < mostFreq y) l
  where mostFreq l = snd . last . quickSort $ map pairFlip $ histogram l

--------------------------------------------------------------------
--   Зад.3

-- по списък с наредени двойки (в случая Item-и)
-- намираме тази, за която snd е най-малкото.
minSnd :: [Item] -> String
minSnd [] = error "Empty list!"
minSnd (x:xs) = helper x xs
  where helper m [] = fst m
        helper m (i:is)
          | snd m < snd i = helper m is
          | otherwise     = helper i is

-- отново можем да го направим с нашата универсална функция за сравнение
minSnd' :: [Item] -> String
minSnd' items = fst . head $ quickSort' (\x y -> snd x < snd y) items

type Item = (String, Int)

expiringItems :: [Item] -> (String,Int,String)
expiringItems items = (almostExpired, numExpired, longestExpired)
  where almostExpired = minSnd [ i | i<-items, (snd i)>=0 ]
        numExpired = length [ i | i<-items, (snd i)<0 ]
        longestExpired = minSnd items

items :: [Item]
items =  [("Milk",3), ("Bread",1), ("Yoghurt",-3),
          ("Butter",5), ("Cheese",-1), ("Pasta",2)]

-- средно-аритметично на числа в списък
average :: [Double] -> Double
average xs = sum xs / (fromIntegral (length xs))

-- средната стойност на компания по дадено име
companyAverage :: String -> [Quote] -> Double
companyAverage name quotes = average [ snd q | q<-quotes, (fst q) == name ]

type Quote = (String, Double)
bestCompany :: [Quote] -> (String, Double, Double)
bestCompany quotes = (highestAvgName, minQuote, maxQuote)
  where allNames = removeDuplicates [ fst q | q<-quotes ] -- събираме всички имена в списък
        quoteAverages = [ (companyAverage n quotes, n) | n<-allNames ] -- за всяко име съпоставяме средната стойност
        highestAvgName = snd $ maximum quoteAverages  -- на този списък директно взимаме максимума
        minQuote = minimum [ (snd q) | q<-quotes, (fst q)==highestAvgName ] -- след като сме намерили името, останалото е просто филтриране
        maxQuote = maximum [ (snd q) | q<-quotes, (fst q)==highestAvgName ]

quotes :: [Quote]
quotes = [("Acme",2.56), ("Buy'n'Large",12.5), ("Acme",42), ("Smiths",9.8),
          ("Buy'n'Large",13.37), ("Acme",10.4), ("Smiths",10.6)]

--------------------------------------------------------------------------
--   Зад.4

data Tree = Empty | Node Integer Tree Tree

prune :: Tree -> Tree
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node value left right) = Node value (prune left) (prune right)


bloom :: Tree -> Tree
bloom Empty = Empty
bloom (Node value Empty Empty) = Node value (makeLeaf value) (makeLeaf value)
  where makeLeaf value = Node value Empty Empty
bloom (Node value left right)  = Node value (bloom left) (bloom right)
  
{-
(define (makeTree val l r) (list val l r))
(define (root t) (car t))
(define (left t) (cadr t))
(define (right t) (caddr t))
(define (empty? t) (null? t))
(define (isLeaf? t) (and (empty? (left t)) (empty? (right t))))

(define (prune t)
  (cond [(empty? t) '()]
        [(isLeaf? t) '()]
        [else (makeTree (root t) (prune (left t)) (prune (right t)))]))

(define (makeLeaf val) (makeTree val '() '()))
(define (bloom t)
  (cond [(empty? t) '()]
        [(isLeaf? t) (makeTree (root t) (makeLeaf (root t)) (makeLeaf (root t)))]
        [else (makeTree (root t) (bloom (left t)) (bloom (right t)))]))

-}