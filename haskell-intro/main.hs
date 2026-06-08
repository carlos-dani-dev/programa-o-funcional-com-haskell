-- respostas às questões do arquivo [intro à haskell.txt]

-- questão 1
divisoresAux :: Int -> Int -> [Int]
divisoresAux x contador
    |contador >= x = []
    |x `mod` contador == 0 = contador : divisoresAux x (contador+1)
    |x `mod` contador /= 0 = divisoresAux x (contador+1)

divisores :: Int -> [Int]
divisores x
    |x <= 0 = []
    |otherwise = divisoresAux x 1

somaDivisores :: Int -> Int
somaDivisores x
    |x <= 0 = 0
    |otherwise = sum (divisores x)

ehPerfeito :: Int -> Bool
ehPerfeito x
    |x <= 0 = False
    |somaDivisores x == x = True
    |otherwise = False

--questão 2
ehPrimo :: Int -> Bool
ehPrimo x
    |x <= 0 = False
    |somaDivisores x == 1 = True
    |otherwise = False

--questão 3
fatorial :: Int -> Int
fatorial x
    |x == 0 || x == 1 = 1
    |x >1 = x*fatorial((x-1))

--questão 4
q4Aux :: Int -> Int -> Int -> Int
q4Aux m n contador
    |(m+contador) <= n = (m+contador) * q4Aux m n (contador+1)
    |otherwise = 1

q4 :: Int -> Int -> Int
q4 m n
    |m>n = 0
    |m==n = m*n
    |otherwise = q4Aux m n 0

--questão 5
fib :: Int -> Int
fib m
    |m == 1 = 0
    |m == 2 = 1
    |m > 2 = fib (m-1) + fib (m-2)

--questão 6
resto :: Int -> Int -> Int
resto m n
    |m>=n = resto (m-n) n
    |m<n = m

--questão 7
divInteiraAux :: Int -> Int -> Int -> Int
divInteiraAux m n contador
    |m >= n = divInteiraAux (m-n) n (contador+1)
    |m < n = contador

divInteira :: Int -> Int -> Int
divInteira m n
    |m < n = 0
    |otherwise = divInteiraAux (m-n) n 1