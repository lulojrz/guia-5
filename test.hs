module Test where

import Test.HUnit
import GUIA

run = runTestTT testSacarBlancosRepetidos

testlongitud = test[
   "longitud [1,2,3]:"~:(longitud [1,2,3])~?=3,
   "longitud []:"~:(longitud [])~?=0

  ]

testultimo= test[
   "ultimo [1,2,3]:"~:(ultimo [1,2,3])~?=3,
   "ultimo [3,2,1]:"~:(ultimo [3,2,1])~?=1

  ]  


testprincipio= test[
   "principio [1,2,3]:"~:(principio [1,2,3])~?=[1,2],
   "principio [3,2,1]:"~:(principio [3,2,1])~?=[3,2]

  ]  


testReverso = test[
   "reverso [1,2,3]:"~:(reverso [1,2,3])~?=[3,2,1],
   "reverso [3,2,1]:"~:(reverso [3,2,1])~?=[1,2,3] 
   ]


testPertenece = test[
   "pertenece 3[1,2,3]:"~:(pertenece 3 [1,2,3])~?=True,
   "pertenece 3[4,2,1]:"~:(pertenece 3 [4,2,1])~?=False 
   ]

testTodosIguales = test[
   "todos Iguales [1,1,1]:"~:(todosIguales [1,1,1])~?=True,
   "todos Iguales [30,20,3]:"~:(todosIguales [30,20,3])~?=False 
 
 ]

testTodosDistintos = test[
   "todos distintos [1,1,1]:"~:(todosDistintos [1,1,1])~?=False,
   "todos distintos [30,20,3]:"~:(todosDistintos [30,20,3])~?=True 
 
 ]

testhayrepetidos = test[
   "hay repetidos [1,1,1]:"~:(hayRepetidos [1,1,1])~?=True,
   "hay repetidos [30,20,3]:"~:(hayRepetidos [30,20,3])~?=False 
 
 ]

testQuitarNumero = test[
   "quitar numero 3 [5,5,3,4,5]:"~:(quitarNumero 3 [5,5,3,4,5])~?=[5,5,4,5]

 ]



testQuitarNumeros = test[
   "quitarTodos 1 [1,1,1]:"~:(quitarTodos 1  [1,1,1])~?=[],
   "quitarTodos 3 [30,20,3]:"~:(quitarTodos 3 [30,20,3])~?=[30,20],
   "quitarTodos 40 [30,20,3]:"~:(quitarTodos 40 [30,20,3])~?=[30,20,3] 

 ] 

testEliminarRepetidos = test[
   "eliminar 1 en [30,1,1,2]"~:(eliminarRepetidos [30,1,1,2])~?=[30,1,2]

 ]

testCapicua = test[
   "capicua [5,4,4,5]"~:(capicua [5,4,4,5])~?=True,
   "capicua [3,2,1]"~:(capicua [3,2,1])~?=False
 ]

testSumatoria = test[
   "sumatoria [5,4,4,5]"~:(sumatoria [5,4,4,5])~?=18
 ]

testproductoria = test[
   "productoria [5,2,5]"~:(productoria [5,2,5])~?=50
 ]


testMaximo = test[
   "maximo [5,20,5]"~:(maximo [5,20,5])~?=20
 ]

testSumarN = test[
   "sumar 3 [1,1,1]"~:(sumarN 3 [1,1,1])~?=[4,4,4]
 ]

testSumarElPrimero = test[
   "sumar primero [1,3,4,5,6]"~:(sumarElPrimero [1,3,4,5,6])~?=[2,4,5,6,7]
 ]

testSumarElultimo = test[
   "sumar ultimo [1,3,4,5,6]"~:(sumarElUltimo [1,3,4,5,6])~?=[7,9,10,11,12]
 ]

testPares = test[
   "pares [1,3,4,5,6]"~:(pares [1,3,4,5,6])~?=[4,6]
 ]

testMultiplosdeN = test[
   "multiplosdeN 3 [6,9,10]"~:(multiplosDeN 3 [6,9,10])~?= [6,9]
  ]


testOrdenar = test[
   "ordenar[8,11,4]"~:(ordenar [8,11,4])~?= [4,8,11]
 ]

testSacarBlancosRepetidos = test[
   "hola   mundi"~:(sacarBlancosRepetidos "hola    mundo")~?= "hola mundo"
 
 ]