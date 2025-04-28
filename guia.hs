--ejercicio 1
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo( xs)


principio :: [t] -> [t]
principio[t] = []
principio(x:xs) = x: principio xs


reverso::[t] -> [t]
reverso [t] = [t]
reverso t = ultimo t : reverso(principio t)

sacarUltimo :: [t] -> [t]
sacarUltimo [t]= [] 
sacarUltimo (x:xs) = x : sacarUltimo xs 


--ejercicio 2 


pertenece::(Eq t) => t -> [t] -> Bool
pertenece t [a] = (t==a)
pertenece t (x:xs) | t== x = True
                   | otherwise = pertenece t xs
                                  
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [a,b] = a == b
todosIguales (x:xs) = (x== head(xs)) && (todosIguales xs)

todosDistintos::(Eq t) => [t] -> Bool
todosDistintos [a,b] = a/=b 
todosDistintos (x:xs) = (x /= head(xs)) && (todosDistintos xs)


hayRepetidos :: (Eq t ) => [t] -> Bool
hayRepetidos [a,b] = a==b
hayRepetidos (x:xs ) = (x== head xs ) || (hayRepetidos xs)


quitar:: (Eq t ) => t -> [t] -> [t]
quitar elem [] = []
quitar elem (x:xs) | elem == x = xs
                   | otherwise = (x: quitar elem xs)

{--quitarTodos :: (Eq t) => t -> [t] ->[t]
quitarTodos y [a] | y==a = []
                  | otherwise = [a]
quitarTodos y (x:xs) | y == x = quitar y xs
                     | otherwise = y: quitarTodos y tail --}
 



--ejercicio 3
sumatoria::[Integer] -> Integer
sumatoria [] = 0  
sumatoria (x:xs) =  x + sumatoria (xs)

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x* productoria xs


maximo:: [Integer] -> Integer
maximo[x] = x
maximo (x:y:xs) | x>y  = maximo(x:xs)
                 | otherwise = maximo (y:xs)


sumarN :: Integer ->[Integer] -> [Integer]
sumarN n [t] = [n+t]
sumarN n (x:xs) = x+n : sumarN n xs

sumarPrimero :: [Integer] -> [Integer]
sumarPrimero (x:xs)= sumarN x (x:xs)   






ordenar::[Integer] -> [Integer]
ordenar [] = []
ordenar a = ordenar( quitar (maximo a ) a)  ++ [maximo a]