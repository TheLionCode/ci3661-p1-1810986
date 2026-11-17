# Aventura

Motor de aventuras de texto para CI-3661.

Gabriel Carballo, carnet 18-10986. Trabajo individual.

Variante **Turnos**. La cuenta es 1810986 módulo 4 = 2.

## Compilar y correr

Con Cabal:

```bash
cabal build
cabal exec aventura -- mundos/mundo.txt
cabal exec aventura -- --validar mundos/mundo.txt
cabal test
```

El guion que gana el mundo propio:

```bash
cabal exec aventura -- mundos/mundo.txt < guiones/mundo.txt
```

El del material de ejemplo:

```bash
cabal exec aventura -- mundos/entrega.txt < guiones/entrega.txt
```

`aventura MUNDO` empieza la partida. `aventura --validar MUNDO` imprime `Mundo válido.` si el archivo sirve, o los errores y termina con código 1.

## Pruebas

`cabal test` corre `test/Pruebas.hs` (HUnit): el mundo de ejemplo, el propio, los inválidos del material, los de `mundos/invalidos/`, comandos y secuencias de `ejecutar` sin entrada ni salida.
