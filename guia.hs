module GUIA where
--ejercicio 1
--1
longitud :: [t]-> Integer
longitud [] = 0
longitud(x:xs) = 1 + longitud(xs)
--2
ultimo :: [t]-> t 
ultimo [x] = x
ultimo (x:xs) = ultimo(xs)
--3
--devuelve toda la lista menos el ultimo
principio :: [t]-> [t]
principio [t] = []
principio(x:xs) = x:principio xs


--4
reverso :: [t]-> [t] 
reverso [x] = [x]
reverso(x:xs) = ultimo xs : reverso(principio (x:xs))



--ejercicio 2.1
pertenece :: (Eq t) => t-> [t]-> Bool
pertenece _ [] = False
pertenece num  (x:xs)| num == x = True
                     | otherwise = pertenece num xs

--2.2
todosIguales :: (Eq t) => [t]-> Bool
todosIguales [x] = True
todosIguales (x:y:xs)| x== y = todosIguales(y:xs)
                     | otherwise = False

--2.3
todosDistintos :: (Eq t) => [t]-> Bool
todosDistintos [x] = False
todosDistintos(x:y:xs)| x==y = todosDistintos (y:xs)
                      | otherwise = True 
 
--2.4
hayRepetidos :: (Eq t) => [t]-> Bool
hayRepetidos [x] = False
hayRepetidos(x:y:xs)| x== y = True
                    | otherwise = hayRepetidos(y:xs)

--2.5
quitarNumero:: (Eq t) => t-> [t]-> [t]
quitarNumero _ [] = []
quitarNumero num (x:xs) | x== num = xs
                        | otherwise = x: quitarNumero num (xs)


--2.6
quitarTodos :: (Eq t) => t-> [t]-> [t]
quitarTodos _ [] = []
quitarTodos num (x:xs)| x==num = quitarTodos num (xs)
                      | otherwise = x:quitarTodos num (xs)



--2.7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [t] = [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

--2.8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos lista_a lista_b = (contiene lista_a lista_b) && (contiene lista_b lista_a)

contiene :: (Eq t) => [t] -> [t] -> Bool
contiene [a] lista_b = pertenece a lista_b
contiene lista_a lista_b = (pertenece (head (lista_a)) lista_b) &&  (contiene (tail lista_a) lista_b)

--2.9
capicua :: (Eq t) => [t]-> Bool 
capicua x = reverso x == x



--3

sumatoria::[Integer]->Integer
sumatoria[]=0
sumatoria (x:xs) = x+sumatoria(xs)

productoria :: [Integer]-> Integer
productoria[]=1
productoria (x:xs) = x*productoria(xs) 


maximo :: [Integer]-> Integer 
maximo[x]=x
maximo(x:y:xs)|  x> y = maximo(x:xs)
              | otherwise = maximo(y:xs)



sumarN :: Integer-> [Integer]-> [Integer]
sumarN n [] = []
sumarN num (x:xs) = x+num : sumarN num xs

sumarElPrimero :: [Integer]-> [Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x (x:xs)


sumarElUltimo :: [Integer]-> [Integer]
sumarElUltimo [] = []
sumarElUltimo (x:xs) = sumarN (ultimo(x:xs))  (x:xs)


pares :: [Integer]-> [Integer] 
pares [] = []
pares (x:xs)| mod x 2 == 0 = x:pares(xs)
            | otherwise = pares(xs)


multiplosDeN :: Integer-> [Integer]-> [Integer]
multiplosDeN _ [] = []
multiplosDeN num (x:xs)| mod x num == 0 = x:multiplosDeN num xs 
                       | otherwise = multiplosDeN num xs


ordenar::[Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) = minimo (x:xs) : ordenar(quitarNumero (minimo(x:xs)) (x:xs))


minimo :: [Integer]-> Integer 
minimo[x]=x
minimo(x:y:xs)|  y> x = minimo(x:xs)
              | otherwise = minimo(y:xs)





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
