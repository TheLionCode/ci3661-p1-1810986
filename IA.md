# Uso de IA

Para revisar el código sin salir del editor usé Visual Studio Code con Codeium, la extensión gratuita. El chat puede mirar el archivo que tengo abierto y comentar esa parte ahí mismo, sin copiarla a otra página. Me sirvió para leer funciones que ya estaban escritas y ver si el tipo o el mensaje de error coincidían con lo que yo creía.

También me apoyé en Gemini, la versión web. Le pasaba bloques sueltos (un módulo o una función que no compilaba, o un pedazo de `leerMundo` / `ejecutar`) para ir más rápido y para salir de errores de tipos y de lógica. Yo armaba el archivo, pegaba el trozo, lo compilaba y corregía lo que no coincidía con el enunciado.

La parte que más revisé con eso fue el reconocedor, la validación que tiene que devolver todos los errores juntos, los comandos y el núcleo. El mundo propio, los inválidos de `mundos/invalidos/` y estos textos los cerré yo con el mismo criterio.

En una conversación informal Gemini me propuso reconocer el archivo de mundo con Parsec. El bosquejo era un `import Text.Parsec` y un parser con `do` que devolvía el `Mundo` o un `ParseError`. Es lo que suele recomendarse en Haskell para una gramática de claves y bloques, y el ejemplo se veía más corto que encadenar `luego`.

No lo usé. El enunciado prohíbe Parsec, megaparsec, attoparsec, ReadP y expresiones regulares. Además el número de línea no me hacía falta dentro del combinador: `preparar` en `Motor.Mundo` numera las líneas al partir el texto, y `Motor.Reconocedor` se queda en `ReadS` con `reconocer`, que solo acepta un resultado si consume la entrada entera. Si hubiera seguido esa sugerencia, el proyecto quedaba fuera de las reglas y mezclaba el reporte de errores (que tiene que acumular varios, no parar en el primero) con el `ParseError` de una librería que no puedo depender. Lo dejé como está: combinadores propios y la lista de `ErrorMundo` en `leerMundo`.
