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