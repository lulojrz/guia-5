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


quitarTodos::  (Eq t) => t -> [t] -> [t]
quitarTodos elem [a] | (elem == a) = []
                     | otherwise = [a]

quitarTodos elem (x:xs) | (elem == x) = quitarTodos elem xs
                        | otherwise = x : (quitarTodos elem xs)


                        
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [t] = [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)


mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos lista_a lista_b = (contiene lista_a lista_b) && (contiene lista_b lista_a)

contiene :: (Eq t) => [t] -> [t] -> Bool
contiene [a] lista_b = pertenece a lista_b
contiene lista_a lista_b = (pertenece (head (lista_a)) lista_b) &&  (contiene (tail lista_a) lista_b)

capicua :: (Eq t) => [t] -> Bool
capicua ls = (reverso ls == ls)


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

sumarN :: Num t => t ->[t] -> [t]
sumarN n [t] = [n+t]
sumarN n (x:xs) = x+n : sumarN n xs

sumarPrimero :: [Integer] -> [Integer]
sumarPrimero (x:xs)= sumarN x (x:xs)   



sumarUltimo::  [Integer] -> [Integer]
sumarUltimo (x:xs) = sumarN (ultimo(x:xs) ) (x:xs)




pares :: [Integer] -> [Integer]
pares lista = multiplosDeN 2 lista

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []  -- caso base necesario
multiplosDeN n (x:xs)
  | mod x n == 0 = x : multiplosDeN n xs
  | otherwise    = multiplosDeN n xs


ordenar::[Integer] -> [Integer]
ordenar [] = []
ordenar a = ordenar( quitar (maximo a ) a)  ++ [maximo a]


--[4]

-- 4.1
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [t] = [t]
sacarBlancosRepetidos (x:xs) | (x == ' ') && (head xs == ' ') = sacarBlancosRepetidos xs
                             | otherwise = x : sacarBlancosRepetidos xs


--4.2
contarPalabras :: [Char]-> Integer
contarPalabras [] = 0
contarPalabras (x:xs) = 1 + contarPalabras(xs)


--4.3
--no lo entendi

--4.4


-- 4.5

--4.6


--5
sumaAcumulada::(Num t) => [t]-> [t]
sumaAcumulada [t] = [t] 
sumaAcumulada (x:xs) = x: sumarN x (sumaAcumulada xs) 

lista = [1,2,3,4,5]



--6
type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos ::Nombre ->ContactosTel ->Bool
enLosContactos _ [] = False

enLosContactos nombre ((contactoNombre, _): contactos) = nombre == contactoNombre || enLosContactos nombre contactos


agregarContacto :: Contacto -> ContactosTel -> ContactosTel 
agregarContacto unContacto [] = [unContacto]
agregarContacto (nuevoNom,nuevoTel) ((contactoNombre,contactoTel):miagenda) | (nuevoNom ) == (contactoNombre) = ((contactoNombre,nuevoTel):miagenda)
                                                     | otherwise = (contactoNombre,contactoTel) : (agregarContacto (nuevoNom,nuevoTel) miagenda)


eliminarContacto :: Nombre->ContactosTel->ContactosTel
eliminarContacto _ [] = []
eliminarContacto eliminado ((nombre,telefono) : miagenda) | eliminado == nombre = eliminarContacto eliminado miagenda
                                                          | otherwise = (nombre,telefono) : eliminarContacto eliminado miagenda


miagenda = [("vir","123"),("simon","1254125")]



--7
type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

lockers =
 [
 (100,(False,"ZD39I")),
 (101,(True,"JAH3I")),
 (103,(True,"IQSA9")),
 (105,(True,"QOTSA")),
 (109,(False,"893JJ")),
 (110,(False,"99292"))
 ]



 --7.1
existeElLocker:: Identificacion->MapaDeLockers->Bool
existeElLocker _ [] = False
existeElLocker codigo ((num,_):xs) = codigo == num || existeElLocker codigo xs


--7.2
ubicacionDelLocker :: Identificacion->MapaDeLockers->Ubicacion
ubicacionDelLocker _ [] = " "

ubicacionDelLocker codigo ((num,(_,ubi)):xs) | codigo == num = ubi
                                             | otherwise = ubicacionDelLocker codigo xs


--7.3
estaDisponibleElLocker :: Identificacion->MapaDeLockers->Bool
estaDisponibleElLocker _ [] = False
estaDisponibleElLocker codigo ((num,(bool,_)) :xs) | codigo==num = bool
                                                   | otherwise = estaDisponibleElLocker codigo xs

--7.4
ocuparLocker :: Identificacion->MapaDeLockers->MapaDeLockers
ocuparLocker _ [] = []
ocuparLocker codigo ((num,(bool,id)):xs) | bool == False = ((num,(True,id)):xs)
                                         | otherwise =  (num,(bool,id)) : ocuparLocker codigo xs                  
