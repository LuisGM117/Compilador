# Compilador

## Como arrancar

En consola teclear:

$ `python3 Compilador.py`

## Variables 

$ `VAR <var_name> = <value>` 

$ `VAR <var_name2> = <value>`

**EJEMPLOS**

$ `VAR a = 10`

$ `VAR b = 5.0`

Puede ser enteros o float

## Operaciones

$ `<var_name> + | - | * | ^ <var_name2>`

**EJEMPLOS**

Con las variables declaradas arriba

$ `a + b`

$ `15.0`

$ `VAR C = a - b` 

$ `5.0`

## Comparar

$ `<var_name> > | < | <= | >= <var_name2>`

**EJEMPLOS** 

Con las variables que declaramos arriba

$ `a > b`

$ `1` que significa verdadero

$ `a < b`

$ `0` que significa falso




## Booleans

$ `<var_name> AND | OR <var_name2>`

$ `a > b AND a < 117`

$ 1 que significa verdadero


## Condicionales

**IF**

$ `IF <condition> THEN <expresssion> ELIF <condition> THEN <expresion>`

**EJEMPLO**

$ `FOR i = 1 TO 6 THEN VAR result = result * i`


**WHILE**S

$ `WHILE <condition> THEN <expresion>`

**EJEMPLO**

$ `VAR i = 0`

$ `WHILE i < 10000 THEN VAR i = i + 1`

$ `i`

---

## Repositorio para el proyecto final de la materia de Compiladores.


Para proyecto final de esta materia crearemos un pequeño compilador, para un lenguaje con las siguientes funcionalidades:

**Operaciones permitidas:**

Aritméticas:
1. Suma +
2. Resta -
3. Multiplicación *
4. División /
5. Exponenciación ^

Comparación:

1. ==
2. != 
3. ~~>~~                       
4. <
5. ~~>=~~
6. <=

Booleanas:
1. and 
2. or

Operaciones de bloques:
1. ( )
2. {}

Un sistema de tipos:
1. Int
2. Float
3. String
4. Bolean

Operaciones permitidas entre el sistema de tipos:




Para la parte de análisis sintáctico https://www.dabeaz.com/ply/ply.html (Enlaces a un sitio externo.)

Ejemplos: https://github.com/dabeaz/ply/blob/master/example/

 

Para el codigo de tres direcciones se utilizaran las siguietes instruciones:
