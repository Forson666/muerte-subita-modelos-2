module Simulacro where

-- devuelve  un entero con los ultimos digitos de una lista de enteros
ultimosdig:: [Int] -> Int
ultimosdig [] = 0
ultimosdig (x:xs) = ((mod x 10) * (10 ^ (length xs))) + ultimosdig xs

-- Halla una lista con los enteros formados por los multiplos de 3 de los digitos de los enteros de una lista

intalist:: Int -> [Int]
intalist 0 = []
intalist a | a < 10 = [a] | otherwise = (intalist (div a 10)) ++ [mod a 10]

multi3:: [Int] -> [Int]
multi3 [] = []
multi3 lista = [ x | x <- lista , mod x 3 == 0]

listaint:: [Int] -> Int
listaint [] = 0
listaint(x:xs) = (x * (10 ^ (length xs))) + listaint xs

listmulti3:: [Int] -> [Int]
listmulti3 [] = []
listmulti3 lista = [listaint (multi3 (intalist x)) | x <- lista]

-- Para una lista de listas devuelve una lista de tuplas con el mayor, menor y la suma del mayor y menor de cada sublista
mayor:: [Int] -> Int
mayor [x] = x
mayor (x:xs) | x > mayor xs = x | otherwise = mayor xs

menor:: [Int] -> Int
menor [x] = x
menor (x:xs) | x < menor xs = x | otherwise = menor xs

hallarTupla::[[Int]]->[(Int,Int,Int)]
hallarTupla lista = [(menor x ,mayor x ,mayor x + menor x)| x <- lista]
