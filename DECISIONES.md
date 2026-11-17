# Decisiones de diseño

Trabajo individual. Carnet 18-10986.

## D1. Cómo se representa el mundo y los nombres

Contexto. Una sala y un objeto pueden llamarse igual, y durante el juego hay que buscarlos todo el tiempo: en qué sala está el jugador, a dónde lleva una salida, si ese objeto existe. Si todo queda como un texto cualquiera, un título y un nombre de sala se ven igual. Y si las salas van en una lista, cada `ir` tiene que recorrerla.

Opciones. Podía dejar los nombres como `String` y las salas en una lista. También pensé en un solo mapa para salas y objetos juntos. La otra era separar el nombre en su propio tipo y tener un mapa de salas y otro de objetos.

Decisión. Quedó un `newtype Id` en `Motor.Tipos`, y `mundoSalas` y `mundoObjetos` son mapas de ese `Id`. Las salidas de una sala no van en un mapa: van en una lista, igual que los objetos que hay en la sala. El enunciado dice que salas y objetos no comparten nombres, así que `llave` puede ser las dos cosas y por eso no se mezclan.

Consecuencias. Buscar una sala es directo y un título no se confunde con un identificador. El tipo, igual, no avisa si hay dos salas con el mismo nombre, si una salida apunta a un sitio que no existe o si hay una sala a la que no se llega: eso lo revisa `validar`, dentro de `leerMundo`, antes de armar el mapa. Si las salidas fueran un mapa, la segunda salida al norte se perdería y no se podría decir que hay dos. Recorrer la lista es un poco más incómodo, pero en estas aventuras cada sala tiene pocas salidas.

## D2. Qué guarda el estado de la partida

Contexto. Al soltar un objeto hay que saber en qué sala queda. Al cargar una partida tampoco conviene volver a leer el archivo del mundo solo para recuperar las descripciones. El inventario no alcanza: si el carnet se deja en otra sala, ya no está ni en su sitio original ni en las manos.

Opciones. Una era copiar el mundo completo en cada turno y, aparte, anotar la sala, el inventario y dónde quedó cada objeto. La otra era no copiar el mundo y guardar solo lo que cambió, reconstruyendo la posición cada vez que se mira alrededor.

Decisión. `Estado`, en `Motor.Tipos`, se queda con el mundo entero, la sala actual, el inventario, un mapa de los objetos que siguen en alguna sala, los turnos que quedan y si la partida ya terminó. Los objetos del inventario no aparecen en ese mapa. `estadoInicial` lo arma a partir de lo que dice cada sala. Cuando se suelta algo, `Motor.Nucleo` lo saca del inventario y lo pone en la sala actual.

Consecuencias. Mirar o examinar no tiene que reconstruir nada, y soltar tiene un lugar claro. El estado carga con todas las descripciones aunque en un turno casi no cambien. Para los mundos de este proyecto no pesa. La partida que se guarda en disco sí es más corta, y eso está en la decisión de guardar.

## D3. Cómo se reportan todos los errores y no solo el primero

Contexto. El archivo `invalido_semantica.txt` trae nueve problemas a la vez. `leerMundo` devuelve o el mundo o una lista de errores. Si eso se encadena con un `do`, el primer error corta y el resto no sale.

Opciones. El `do` es lo más corto de escribir. También se podía parar en el primer error de cada bloque y seguir con el siguiente, pero igual se pierden avisos. La otra era recorrer todo el archivo y juntar los mensajes en una lista.

Decisión. En `Motor.Mundo` primero se juntan los errores de forma, línea por línea. Solo si esa lista queda vacía se pasa a revisar salas, salidas y objetos, y ahí se suma el resto. No hay un `do` de `Either` en ese camino. Si el archivo está mal escrito, no se intenta interpretar el significado: `invalido_sintaxis.txt` se queda en sus cuatro errores.

Consecuencias. El mundo roto del material muestra los nueve errores juntos, y las pruebas pueden contarlos. `validar` queda más largo que un `do`, y los mensajes salen en el orden del archivo, no por importancia. Si más adelante hace falta otro chequeo, se agrega a esa lista. Metido en medio de un `Either`, los que vienen después dejarían de aparecer.

## D4. Cómo está hecho el reconocedor

Contexto. Hay que leer el mundo y los comandos sin Parsec, sin ReadP y sin expresiones regulares. Un error de sintaxis tiene que decir el número de línea, y un comando como `ir norte` no puede dejar texto de más.

Opciones. Podía envolver `ReadS` en un tipo propio para escribir el lector con `do`. O usar `ReadS` directo, con funciones pequeñas. Para un texto que se puede leer de más de una forma, se podían guardar todas las lecturas o exigir que una sola se coma la línea entera.

Decisión. En `Motor.Reconocedor` quedaron `falla`, `exito`, `cadena`, `token`, `unoDe`, `luego` y las demás sobre `ReadS`, sin tipo nuevo. `reconocer` se queda con la primera lectura que no deja nada sobrando. El número de línea no sale de ahí: `preparar`, en `Motor.Mundo`, numera las líneas al partir el archivo y el error guarda ese número. Los comandos reutilizan las mismas funciones. Después de un verbo hace falta un espacio o el fin de la línea, para que `guardarfoo` no cuente como `guardar`.

Consecuencias. No hay una librería que el enunciado no deja, y el mismo lector sirve para el mundo y para los comandos. Escribir las secuencias con `luego` es más incómodo que un `do`. A cambio no hay que mantener instancias, y si sobra texto el comando simplemente no se reconoce. También se acepta la dirección sola, como `oeste`, porque el guion de ejemplo la usa. Si solo se aceptara `ir oeste`, ese guion no ganaría.

## D5. La variante de turnos

Contexto. 1810986 módulo 4 da 2, así que la variante es turnos. El mundo de ejemplo no trae esa clave, y un mundo cualquiera tampoco tiene por qué traerla. Si un `ir` choca con una puerta y no gasta turno, el límite no sirve de nada. Y si el movimiento que cumple la meta es justo el último, eso no puede contarse como derrota.

Opciones. Podía exigir `TURNOS` en todos los archivos, o dejarlo solo cuando el mundo lo trae. Podía cobrar turno en cualquier comando, o solo en los que intentan cambiar algo: `ir`, `tomar`, `soltar` y `examinar`, también cuando fallan. Al llegar a cero, o gana siempre el tiempo, o gana la meta si se cumplió en ese mismo comando.

Decisión. `TURNOS` solo vale en la cabecera y es opcional. Si no está, no hay cuenta. Si está, tiene que ser un entero positivo: `TURNOS: cero` es error y el mensaje nombra a `TURNOS`. Esos cuatro comandos gastan turno. La meta se marca antes de mirar el contador, así que si la partida ya ganó no se agrega el aviso de que se acabó el tiempo. Las claves de las otras variantes siguen siendo desconocidas.

Consecuencias. El ejemplo del material sigue siendo válido. En `mundos/mundo.txt` el guion gana con 9 turnos, y con 8 se acaba al tomar el informe, todavía en la azotea. `mirar` e `inventario` se pueden usar para pensar sin gastar. Un `ir` contra la pared sí gasta. El caso raro es un mundo que ya empieza ganado: ahí el programa imprime la victoria y no entra al bucle, porque no hubo un comando al cual colgarle el mensaje.

## D6. Qué se escribe al guardar la partida

Contexto. Si se guarda el estado entero con `Show`, el archivo se llena de descripciones y no queda claro si esa partida es de este mundo o de otro. Tampoco alcanza con guardar solo la sala y el inventario: si el carnet se suelta en la plaza, al cargar volvería a aparecer en el estacionamiento.

Opciones. Una era `Show` y `Read` de todo el estado. La otra, un archivo corto con lo que cambió respecto del mundo. Dentro de esa, se podía anotar dónde está cada objeto o solo los que ya no están en su sala original.

Decisión. En `Motor.Persistencia` la partida guarda el título, la sala, los turnos que quedan (solo si el mundo tiene límite), el inventario y, con `EN`, los objetos que se movieron de sitio. Al cargar se parte del estado inicial y se aplican esos cambios. Si el archivo no existe, está dañado o el título es de otro mundo, se avisa y la partida sigue. Los turnos que se recuperan son los del archivo, no otra vez el máximo del mundo.

Consecuencias. El archivo se puede leer. Si entre guardar y cargar cambia el título del mundo, la partida se rechaza en vez de reventar con un `read`. No se detecta un cambio más fino: si alguien mueve un objeto dentro del `.txt` y deja el mismo título, el cambio se aplica igual. Para lo que pide el enunciado, y para la prueba de soltar el carnet y volver a cargarlo, alcanza.
