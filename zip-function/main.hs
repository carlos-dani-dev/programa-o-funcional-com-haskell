-- respostas às questões em [exercício de ZF.txt]

--questão 1
teste_q1 :: [Float]
teste_q1 = [7, -1, 0]

verifica :: Int -> String
verifica a
    |a < 0 = "Negativo"
    |a == 0 = "Zero"
    |a > 0 = "Positivo"

q1 :: [Int] -> [(Int, String)]
q1 lista = [(a, verifica(a)) | a <- lista]

--questão 2
paresOrd :: [Int] -> [(Int, Int)]
paresOrd lista = [(a, b) | a <- lista, b <- lista, b /= a]

--questão 3
mediaLista :: [Float] -> Float
mediaLista lista = sum lista / fromIntegral (length lista)

distMedia :: [Float] -> [(Float,Float)]
distMedia lista = [(a, abs(a-mediaLista lista)) | a <- lista]

