module Library where
import PdePreludat

-----Punto 1-----
data ColorBola = Roja | Azul | Verde | Negra deriving(Show, Eq)

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving(Show, Eq)

data Celda = UnaCelda{
    coordenadaXCelda :: Number,
    coordenadaYCelda :: Number, 
    bolasDeCelda :: [ColorBola] 
}deriving (Show, Eq)

data Cabezal = UnCabezal{
    coordenadaXCabezal::Number, 
    coordenadaYCabezal :: Number 
}deriving (Show, Eq)

data Tablero = UnTablero{
    ancho :: Number, 
    alto :: Number, 
    cabezal :: Cabezal,
    celda :: [Celda] 
}deriving (Show, Eq)


-----Punto 2-----
inicializarCabezal::Cabezal
inicializarCabezal = UnCabezal{coordenadaXCabezal = 1, coordenadaYCabezal = 1}

crearCeldaEnPosicion :: (Number,Number) -> Celda
crearCeldaEnPosicion (x,y) = UnaCelda{coordenadaXCelda=x, coordenadaYCelda=y, bolasDeCelda=[]}

inicializarCeldas::Number->Number->[Celda]
inicializarCeldas filas columnas = map crearCeldaEnPosicion [(x,y)|x<-[1..columnas], y<-[1..filas]]
   
inicializarTablero::Number->Number->Tablero
inicializarTablero filas columnas = UnTablero{ancho = columnas, alto = filas, cabezal = inicializarCabezal, celda = inicializarCeldas filas columnas}


-----Punto 3-----

----3)a
mover :: Direccion -> Tablero -> Tablero 
mover direccion tablero = tablero{cabezal = cambiarDireccionDeCabezal (cabezal tablero) direccion} 

cambiarDireccionDeCabezal :: Cabezal -> Direccion -> Cabezal
cambiarDireccionDeCabezal cabezal Arriba = cabezal{coordenadaYCabezal = coordenadaYCabezal cabezal + 1} 
cambiarDireccionDeCabezal cabezal Abajo = cabezal{coordenadaYCabezal = coordenadaYCabezal cabezal - 1} 
cambiarDireccionDeCabezal cabezal Izquierda = cabezal{coordenadaXCabezal = coordenadaXCabezal cabezal - 1} 
cambiarDireccionDeCabezal cabezal Derecha = cabezal{coordenadaXCabezal = coordenadaXCabezal cabezal + 1} 
----3)b
esActual::Cabezal->Celda->Bool
esActual cabezal celda = (coordenadaXCelda celda == coordenadaXCabezal cabezal) && (coordenadaYCelda celda == coordenadaYCabezal cabezal) 

poner :: ColorBola -> Tablero -> Tablero
poner bolita tablero = tablero{celda = celdasConBolitaModificada tablero bolita agregarBolita}

celdasConBolitaModificada::Tablero->ColorBola->(ColorBola->Celda->Celda)->[Celda]
celdasConBolitaModificada tablero bolita accionBolita = (celdaActualConBolitaModificada bolita tablero accionBolita) : celdasNoActuales tablero

celdaActualConBolitaModificada::ColorBola-> Tablero->(ColorBola->Celda->Celda)->Celda
celdaActualConBolitaModificada bolita tablero accionBolita = ( (accionBolita bolita) . celdaActual) tablero 

celdaActual :: Tablero->Celda
celdaActual tablero = head(filter (esActual (cabezal tablero)) (celda tablero))

agregarBolita::ColorBola->Celda->Celda
agregarBolita bolita celda = celda{bolasDeCelda = bolita:(bolasDeCelda celda)}

celdasNoActuales::Tablero->[Celda]
celdasNoActuales tablero = filter (not.(esActual (cabezal tablero))) (celda tablero)
----3)c
sacar :: ColorBola -> Tablero -> Tablero
sacar bolita tablero = tablero{celda = celdasConBolitaModificada tablero bolita sacarBolita}

sacarBolita::ColorBola->Celda->Celda
sacarBolita bolita celda = celda{bolasDeCelda = (drop 1 (obtenerBolasDeColor celda bolita))++(obtenerBolasDeDistintoColor celda bolita)}

obtenerBolasDeDistintoColor::Celda->ColorBola->[ColorBola]
obtenerBolasDeDistintoColor celda color =  filter (/= color) (bolasDeCelda celda)

obtenerBolasDeColor::Celda->ColorBola->[ColorBola]
obtenerBolasDeColor celda color =  filter (== color) (bolasDeCelda celda)


-----Punto 4-----

----4)a
aplicarSentencias::Tablero->[(Tablero->Tablero)]->Tablero
aplicarSentencias tablero sentencia
    |length sentencia >=1 = aplicarSentencias ((head sentencia) tablero)  (tail sentencia) 
    |otherwise = tablero
si::(Tablero->Bool)->[(Tablero->Tablero)]->Tablero->Tablero
si condicion sentencias tablero
    |condicion tablero = aplicarSentencias tablero sentencias 
    |otherwise = tablero

sino::(Tablero->Bool)->[(Tablero->Tablero)]->Tablero->Tablero
sino condicion sentencias tablero
    |condicion tablero == False = aplicarSentencias tablero sentencias 
    |otherwise = tablero

alternativa::(Tablero->Bool)->[(Tablero->Tablero)]->[(Tablero->Tablero)]->Tablero->Tablero
alternativa condicion sentenciaverdadera sentenciafalsa tablero
    |condicion tablero = si condicion sentenciaverdadera tablero
    |otherwise = sino condicion sentenciafalsa tablero
----4)b
repetir::Number->[(Tablero->Tablero)]->Tablero->Tablero
repetir cantidad sentencias tablero
 |cantidad>0 = repetir (cantidad-1) sentencias (aplicarSentencias tablero sentencias)
 |otherwise = tablero
----4)c
mientras::(Tablero->Bool)->[(Tablero->Tablero)]->Tablero->Tablero
mientras condicion sentencias tablero
    |condicion tablero = mientras (condicion) sentencias (aplicarSentencias tablero sentencias)
    |otherwise = tablero
----4)d
irAlBorde::Direccion->Tablero->Tablero
irAlBorde direccion tablero
    | (puedeMoverse direccion tablero) == True  = irAlBorde (direccion) ((mover direccion tablero))
    | otherwise = tablero


-----Punto 5----

----5)a
puedeMoverse :: Direccion -> Tablero -> Bool
puedeMoverse direccion tablero = not ((fueraDelTablero . (mover direccion)) tablero) 

fueraDelTablero ::Tablero->Bool
fueraDelTablero tablero = coordenadaXCabezal (cabezal tablero)<1 || coordenadaXCabezal (cabezal tablero)> ancho tablero || coordenadaYCabezal (cabezal tablero)<1 || coordenadaYCabezal (cabezal tablero)> alto tablero
----5)b
hayBolitas::ColorBola->Tablero->Bool
hayBolitas color tablero = length(obtenerBolasDeColor (celdaActual tablero) color)>=1
----5)c
contarCantidadBolitas :: ColorBola -> Tablero -> Number
contarCantidadBolitas color tablero = length (obtenerBolasDeColor (celdaActual tablero) color)


-----Punto 6-----
programa::Tablero->[(Tablero->Tablero)]->Tablero
programa tablero sentenciasordenadas = aplicarSentencias tablero sentenciasordenadas


-----Punto 7-----
tableroDePrueba::Tablero
tableroDePrueba = inicializarTablero 3 3

sentenciasDePrueba::[(Tablero->Tablero)]
sentenciasDePrueba = [
    mover Arriba,
    poner Negra,
    poner Negra, 
    poner Azul, 
    mover Arriba, 
    repetir 15 
        [poner Roja,
        poner Azul
    ], 
    alternativa (hayBolitas Verde) [
        mover Derecha,
        poner Negra
    ] [
        mover Abajo,
        mover Derecha,
        poner Azul
    ], 
    mover Derecha, 
    mientras ( (<=9) . contarCantidadBolitas Verde) [
        poner Verde
    ], 
    poner Azul
 ]
tablerofinal = programa tableroDePrueba sentenciasDePrueba


-----Punto 8-----
tableroVacio::Tablero
tableroVacio = UnTablero{ancho = 3, alto = 3, cabezal = UnCabezal {coordenadaXCabezal = 1, coordenadaYCabezal = 1}, celda = [UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 3, bolasDeCelda = []}]}

tableroBordeDerecha::Tablero
tableroBordeDerecha = UnTablero {ancho = 3, alto = 3, cabezal = UnCabezal {coordenadaXCabezal = 2, coordenadaYCabezal = 1}, celda = [UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 3, bolasDeCelda = []}]} 

tableroDeTestPoner::Tablero
tableroDeTestPoner = UnTablero {ancho = 3, alto = 3, cabezal = UnCabezal {coordenadaXCabezal = 1, coordenadaYCabezal = 1}, celda = [UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 1, bolasDeCelda = [Roja]},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 3, bolasDeCelda = []}]}

tableroDeTestIrAlBorde::Tablero
tableroDeTestIrAlBorde = UnTablero {ancho = 3, alto = 3, cabezal = UnCabezal {coordenadaXCabezal = 3, coordenadaYCabezal = 1}, celda = [UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 3, bolasDeCelda = []}]}

tableroDeTestAlternativa::Tablero
tableroDeTestAlternativa = UnTablero {ancho = 3, alto = 3, cabezal = UnCabezal {coordenadaXCabezal = 1, coordenadaYCabezal = 1}, celda = [UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 1, bolasDeCelda = [Verde,Roja]},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 3, bolasDeCelda = []}]}

tableroDeTestRepetir::Tablero
tableroDeTestRepetir = UnTablero {ancho = 3, alto = 3, cabezal = UnCabezal {coordenadaXCabezal = 1, coordenadaYCabezal = 3}, celda = [UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 3, bolasDeCelda = [Negra]},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 2, bolasDeCelda = [Negra]},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 1, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 3, bolasDeCelda = []}]}

tableroDeTestMientras::Tablero
tableroDeTestMientras = UnTablero {ancho = 3, alto = 3, cabezal = UnCabezal {coordenadaXCabezal = 4, coordenadaYCabezal = 1}, celda = [UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 1, bolasDeCelda = [Roja]},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 1, bolasDeCelda = [Roja]},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 1, bolasDeCelda = [Roja]},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 1, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 2, coordenadaYCelda = 3, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 2, bolasDeCelda = []},UnaCelda {coordenadaXCelda = 3, coordenadaYCelda = 3, bolasDeCelda = []}]}