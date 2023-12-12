module Library where
import PdePreludat

-- Defino mis alias
type Nombre = String
type Fuerza = Number
type Velocidad = Number
type Resistencia = Number
type CapacidadesRobot = (Fuerza, Velocidad, Resistencia)
type CapacidadesVehiculo = (Velocidad, Resistencia)
type Transformacion = CapacidadesRobot -> CapacidadesVehiculo
type Condicion = Autobot -> Bool
type Autobots = [Autobot]
type Nombres = [Nombre]

-- Defino mis tipos 
data Autobot = Robot Nombre CapacidadesRobot Transformacion | Vehiculo Nombre CapacidadesVehiculo deriving (Show)

-- Inicializo algunos Autobots
optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)

jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)

wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)

bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)

autobots = [ optimus, jazz, wheeljack, bumblebee]

-- Defino la función
maximoSegun :: Ord a => (a->a->a) -> a -> a -> a
maximoSegun funcion valor1 valor2
    | funcion valor1 valor2 < funcion valor2 valor1 = valor2
    | otherwise = valor1

-- Defino las funciones que me permiten acceder a las capacidades
robotCapacidades :: Autobot -> CapacidadesRobot
robotCapacidades (Robot _ capacidades _) = capacidades

robotTransformacion :: Autobot -> Transformacion
robotTransformacion (Robot _ _ transformacion) = transformacion 

autobotNombre :: Autobot -> Nombre
autobotNombre (Robot nombre _ _) = nombre
autobotNombre (Vehiculo nombre _) = nombre

autobotFuerza :: Autobot -> Fuerza
autobotFuerza (Robot _ (fuerza, _, _) _) = fuerza
autobotFuerza (Vehiculo _ _) = 0

autobotResistencia :: Autobot -> Resistencia
autobotResistencia (Robot _ (_, _, resistencia) _) = resistencia
autobotResistencia (Vehiculo _ (_, resistencia)) = resistencia

autobotVelocidad :: Autobot -> Velocidad
autobotVelocidad (Robot _ (_, velocidad, _) _) = velocidad
autobotVelocidad (Vehiculo _ (velocidad, _)) = velocidad

-- Defino la función
transformarEnVehiculo :: Autobot -> Autobot
transformarEnVehiculo robot = (Vehiculo (autobotNombre robot) ((robotTransformacion robot) (robotCapacidades robot)))

-- Defino la función
velocidadContra :: Autobot -> Autobot -> Velocidad
velocidadContra autobot1 autobot2 = autobotVelocidad autobot1 - (max (autobotFuerza autobot2 - autobotResistencia autobot1) 0)

-- Defino la función
elMasRapido :: Autobot -> Autobot -> Autobot
elMasRapido autobot1 autobot2 
    | velocidadContra autobot1 autobot2 > velocidadContra autobot2 autobot1 = autobot1
    | otherwise = autobot2

-- Defino las funciones
todasLasFormas :: Autobot -> Autobot -> [(Autobot, Autobot)]
todasLasFormas autobot1 autobot2 = [(autobot1, autobot2), (autobot1, transformarEnVehiculo autobot2), (transformarEnVehiculo autobot1, autobot2), (transformarEnVehiculo autobot1, transformarEnVehiculo autobot2)]

domina :: Autobot -> Autobot -> Bool
domina autobot1 autobot2 = all (==(autobotNombre autobot1)) (map (autobotNombre.uncurry elMasRapido) (todasLasFormas autobot1 autobot2))

losDominaATodos :: Autobot -> Autobots -> Bool
losDominaATodos autobot robots = all (domina autobot) robots

-- Defino la función
quienesCumplen :: Condicion -> Autobots -> Nombres
quienesCumplen condicion robots = map (autobotNombre) (filter condicion robots)

-- Inferir el tipo de la función
-- saraza :: Ord b => b -> b -> b -> (b -> b -> b) -> b
saraza x y w z = z w . maximoSegun z y $ x