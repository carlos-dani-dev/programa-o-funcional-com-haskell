-- respostas às questões em [exercício de ZF.txt]

--questão 1
teste_q1 :: [Int]
teste_q1 = [1, -1, 0]

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

    