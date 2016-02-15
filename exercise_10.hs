square x = x * x

inc x = x + 1

dec x = x - 1

even' x = x `mod` 2 == 0

odd' x = not (even' x)

between n a b = a<=n && n<=b

pyth a b c = a^2 + b^2 == c^2

lucky 7 = "Correct!"
lucky 18 = "Correct!"
lucky _ = "Incorrect!"

factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)