-- respostas às questões em [haskell-lists.txt]

q1 = ["Bom", "Dia", "!"]
q2 = ["Bom\n", "Dia\n", "!\n"]
 
--questão 1
juntaLinha :: [String] -> String
juntaLinha (a:b)
    |b == [] = a
    |otherwise = a++" "++juntaLinha b

--questão 2
juntaLinhaQ2 :: [String] -> String
juntaLinhaQ2 (a:b)
    |b == [] = take ((length a)-1) a
    |otherwise = (take ((length a)-1) a)++" "++juntaLinhaQ2 b

--questão 3

--questão 4

--questão 5
texto = "João disse à Maria que a padaria é perto de casa"
s1 = "a padaria é perto de casa"
s2 = "o  prédio é perto da padaria"

testar_igualdade :: String -> String -> Bool
testar_igualdade [] texto = True -- Se a substring acabou, encontramos o match!
testar_igualdade _ []    = False   -- Se o texto acabou antes, não deu match.
testar_igualdade (aS1:bS1) (aTexto:bTexto)
    | aS1 == aTexto = testar_igualdade bS1 bTexto -- Caractere bateu, continua checando o resto
    | otherwise = False


subst :: String -> String -> String -> String
subst _ _ [] = []
subst s1 s2 (aTexto:bTexto)
    |testar_igualdade s1 (aTexto:bTexto) = s2++ drop (length s1) (aTexto:bTexto) 
    |otherwise = aTexto : subst s1 s2 bTexto