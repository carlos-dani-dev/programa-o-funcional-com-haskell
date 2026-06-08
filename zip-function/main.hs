-- respostas às questões em [exercício de ZF.txt]

--questão 1
teste_q1 :: [Int]
teste_q1 = [1, 2, -1, -10, 0, 0, 1]

verifica :: Int -> String
verifica a
    |a < 0 = "Negativo"
    |a == 0 = "Zero"
    |a > 0 = "Positivo"

q1 :: [Int] -> [(Int, String)]
q1 lista = [(a, verifica(a)) | a <- lista]

--questão 2
