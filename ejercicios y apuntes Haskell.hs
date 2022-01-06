data AnimalFantastico = AnimalFantastico {
	especie :: String,
	peligrosidad :: Int,
	puedeHablar :: Bool
} deriving (Eq, Show)
--este data esta creando 4 funciones, AnimalFantastico que es el constructor, especie que devuelve la especie del animalFantastico, lo mismo para las demas, hay que aclarar que si no usamos esta forma de
--declarar un data, no vamos a poder usar estas funciones sino definimos le data de esta forma (tambien cabe aclarar que estas funciones de declaras el data asi y despues poder usar las funciones especie
--etc, son especiales de ghci, no de haskell) (la funcion especie recibe un AnimalFantastico y devuelve un String, y no puedo despues hacer un data humano que tenga una funcion "especie" igual que esta)

esDocil :: AnimalFantastico -> Bool
puedeSerDomesticado :: AnimalFantastico -> Bool
esPeligroso :: AnimalFantastico -> Bool
esMajestuoso :: AnimalFantastico ->Bool
puedeHablar :: AnimalFantastico -> Bool

Acromantula = AnimalFantastico {
	especie = "Acromantula"
	peligrosidad = 5
	puedeHablar = True
}

Centauro = AnimalFantastico {
	especie = "Centauro"
	peligrosidad = 5
	puedeHablar = True
}

Niffler = AnimalFantastico {
	especie = "Niffler"
	peligrosidad = 3
	puedeHablar = False
}


esDocil (AnimalFantastico _ peligrosidad _) = peligrosidad =< 3  --esta funcion recibe un solo paramentro que es de tipo animalFantastico
 
--si yo no hubiese puesto los parentesis en la funcion esDocil : "esDocil AnimalFantastico _ peligrosidad _ = peligrosidad <= 3"
--va a haber un problema porque haskell va a pensar que esdcoil recibe cuatro parametros : una funcion (el AnimalFantastico, porque como esta en mayuscula piensa que es la funcion construcora AnimalFantastico
--que a su vez esta funcion recibe un String Int Bool, que es justamente la que usamos par cuando queremos construir una constante como las de acromantula centauro etc
--y el segundo parametro va a ser un "a" el tercero "b" y  el cuerto un "c", 

--por otro lado, esta funcion (la que esta bien, la que tiene parantesis) que hice yo de esDocil, esta usando pattermatching, que es cuando le pones los parametros que va a recibir para atajarlos toods y despues 
--para despues usar los que te plascan, esta bien usar pattermatching pero le profe dijo que es mejor usar la forma de abajo:
--la forma que esta bien es usar la funcion:  
esDocil a = peligrosidad a =< 3  --(esta usando la funcion peligrosisas que definimos cuando creamos el data de animalFantastico) (no se porque pero la funcion 
-- peligrosisad se pone con minuscula y haskell ya etiende que esa es la funcion dle data)
--importante, aca yo pongo a para hacerlo mas rapido y que se note bien la diferencia entre la forma que esta bien y la que esta mal, pero en los tps o en los parciales, estos nombres de los parametros,
--tienen que ser representatiovs. como por ejemplo animalF (recordar siempre los nombres de parametros en minusculas)


esPeligroso (AnimalFantastico _ 5 _) = True
esPeligroso _ = False 
-- esta forma de arriba esta bien pero como dije antes usa patternmatching y no seria la mejor forma de hacerlo
esPeligroso a = peligrosidad a = 5



puedeHablar (AnimalFantastico _ _ puedeHablar) = puedeHablar
--Creo que esta funcion ni siquiera es necesaria porque en realidad podria usar directamente la funcion puedeHablar que nos ofrece el data


puedeSerDomesticado AnimalFantastico = (not . esPeligroso AnimalFantastico) || (puedeHablar AnimalFantastico && (not . esMajestuoso AnimalFantastico))
--la forma correcta es: 
puedeSerDomesticado a = not (esPeligroso a) || ( puedeHablar a && not (esMajestuoso a))


esMajestuoso (AnimalFantastico especie _ _) = especie == "Unicornio" || especie == "Centauro" || especie == "Sirena"
--la forma correcta de hacer el esmajestuoso:
esMajestuoso a = especie a == "Unicornio" ||
				 especie a == "Sirena" ||
				 especie a == "Centauro"



data magizoologo = magizoologo {
	nombre :: String,
	exp :: Int, 
	ultimaEspecieDomesticado :: String --aca trambien podria ser de tipo AnimalFantastcip pero como esto puede llegar a ser algo vacio, es mas comun que un string sea vacio y no un data entero
}


ganarExp :: Int -> magizoologo -> magizoologo

ganarExp cantidad magizoologo = magizoologo{exp =exp magizoologo + cantidad}  
-- "magizoologo{exp =exp magizoologo + c}" eso no es una funcionl, es importante saberlo, eso solo genera una copia del magizoolgo pero con el cambio an exp que puse

cuidar :: animalFantastico -> magizoolgo -> magizoologo
cuidar animalF magizoologo = ganarExp ( expPorCuidar animalF) magizoologo

expPorCuidar :: animalFantastico -> Int
expPorCuidar a | esDocil a = 3 --nunca usar tab en haskell siempre usar espacios y las guardas tienen que estar alineadas
               | esMajestuoso a = 10
               | otherwise = 7



domestcar :: animalFantastico -> magizoologo -> magizoologo
domesticar animalF magizoologo | puedeSerDomesticado animalF = ganarExp (3*(peligrosidad animalF) magizoologo{ultimaEspecieDomesticado = especie animalF} 
m{ultimaEspecieDomesticado = especie a} --es el segundo parametro que tiene que recibir ganar exp, y no es hacer una funcion que lo cambie sino que
-- lo que estamos haciendo es darle como parametro una copia de una mago modificada para que me despues ganarexp le agrege la exp y listo"
                               | otherwise = ganarexp (-10) (magizoologo)   



--la otra forma es directamente hacer una funcion que cambie el magizoologo de forma que directamente usamos como parametro de ganarexp una funcion, 
--y eso es lo que vamos a tratar de hacer de ahora en adelante

cambiarEspecie :: AnimalFantastico -> Magizoologo -> Magizoologo
cambiarEspecie animalF magizoologo = magizoologo{ ultimaEspecieDomesticado = especie animalF}
--y la funcion domesticar quedaria asi:
domesticar animalF magizoologo | puedeSerDomesticado animalF = ganarExp (3*(peligrosidad animalF)) (cambiarEspecie animalF magizoologo)
                               | otherwise = ganarexp (-10) (magizoologo)



--clase del 25-04-2019:

--composicion:

esGrande n = (mayoA100.doble.sig) n
--saber que ese parentesis es irreemplazable ese parentesis, es el unico que importa, porque si no pongo los parentesis la sentencia se interpreta como
esGrande = mayoA100.doble.sig n
--que doble se compone con el int que devuelve sig que toma como parametro a n, y no se puede hacer ya que el punto tiene que recibir funciones, y de esta forma no interpreta como si recibiera la funcion sig.


--luego de tener claro esto, se puede simplificar la n y dejarlo como
esGrande = (mayoA100.doble.sig) 
--y una vez que queda asi ya es la pedo poner los parentesis y queda
esGrande = mayoA100.doble.sig

--tambien explicaron aplicacion parcial


--clase del 20/05/2019:

--explicaron listas:

--hay dos formas de definirlas:

l :: Persona
l = [] la lista vacia o bien una lista con elementos:
l = [pedro, pepe, luis]

--la otra forma es:

l = [1: 23]

{-
se compone con dos parametros, el primero es la cabeza y el segundo es la cola, una dato importantisimo a tener en cuenta es que:
la cabeza es 1 y es de tipo Int y la cola es de tipo lista, es decir la cola es el primer eleento y la cola es la lista que le sigue a eso.

otra cosa importantisima es que la forma mas baja de declarar la lista (y es de esa menera en como lo piensa haskell): [1: (2 : (3 : [] ) ) ]
es decir la lista esta compuesto de la lista que tiene  como cabeza el elem 1 y tiene  como cola a la lista que tiene como cabeza a 2 y tiene como cola 
a la losta que tiene como cabeza a 3 y tiene como cola a la lista vacia

el profe dijo que lo importantre es saber diferenciar el formato, si es lista vacia o si el lista con cabeza y cola
-}

--explicaron orden superior.


--clase 30/05/2019:

f x y z = head y > map (\n -> n x) z

--deducimos el tipo de la funcion f:

f:: c -> [a] -> [c -> d] -> Bool
    x     y         z (que en realidad el z es una lista de funciones que reciben el mismo tipo que el x)
porque deducimos que z es una lista de funciones? porque el map tiene la funcion lambda que toma un n que ese n va a tomar el x, entonces deducimos que si
ese n que va a tomar es el elemento de z y ese elemento puede tomar como parametro un x, es porque esos elementos de z don funciones que toman un tipo de 
dato que es el mismo que x y devuelve un tiop que no conocemos por eso le ponemos que devuelve un d-

despues si penamos un poco mas, tenemos que ese head va a comparar la lista de y con la lista de z, entonces para poder hacer ese > van a atener que se listas
con los tipos iguales, para que justamente las pueda comparar. entonces:

f:: c -> [[d]] -> [c -> d] -> Bool

y hay otra cosa mas que tiene que ver con los typeclass y con el parrafo anterior porque
para que la lista se puede resolver con el >, implica que tienen que ser ordenables, entonces:

f:: Ord d => c -> [[d]] -> [c -> d] -> Bool