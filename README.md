# Compilador
Repositorio para el proyecto final de la materia de Compiladores.

Para proyecto final de esta materia crearemos un pequeño compilador, para un lenguaje con las siguientes funcionalidades:

Operaciones permitidas:
Aritméticas:
Suma +
Resta -
Multiplicación *
División /
Exponenciación ^
Comparación:
==
!= 
>
<
>=
<=
Booleanas
and 
or
Operaciones de bloques:
( )
{}
Un sistema de tipos:
Int
Float
String
Bolean
Operaciones permitidas entre el sistema de tipos:
int	float	string	boolean
int	Aritmeticas, comparacion	Aritmeticas, comparacion	+ (concatenacion)	and, or, ==, !=
float	Aritmeticas, comparacion	Aritmeticas, comparacion	+ (concatenacion)	and, or, ==, !=
string (OPCIONAL) 	----	----	+ (concatenacion), ==, !=	==, !=
boolean	----	----	----	and, or, ==, !=
Flujos de control existentes, deberan seguir una estructura similar al lenguaje C, por simplicidad todo deberán llevar llaves:
If , else, elif
Screen Shot 2021-11-16 at 23.40.44.png
while () {}.  -- Caracterisitica opcional ( El no entregarla, limitara 1 punto sobre el maximo )
for (;;) {}.    -- Caracterisitica opcional ( El no entregarla, limitara 1 punto sobre el maximo )
Para marcar el final de una sentencia se utilizara ";"
Es permitido el declarar y asignar una variable en la misma linea
Para la entrega de este proyecto es un repositorio de git (puede ser de github, gitlab o bitbucket), los cambios deben ser realizados usando la metodologia gitflow (https://www.atlassian.com/es/git/tutorials/comparing-workflows/gitflow-workflow (Enlaces a un sitio externo.)).


Para la parte de análisis sintáctico https://www.dabeaz.com/ply/ply.html (Enlaces a un sitio externo.)

Ejemplos: https://github.com/dabeaz/ply/blob/master/example/

 

Para el codigo de tres direcciones se utilizaran las siguietes instruciones:

Resultado	Direccion 1	Operador	Direccion 2	Ejemplo
If	Null	Variable con condicion booleana	IFGOTO	Etiqueta	v1 IFGOTO L1
*
/
+
-
^	Variable	Constant/value	*
/
+
-
^	Constant/value	
t1 = 3 * 3

t3 = t1 / t2

toInt

toFloat

Variable	Const/variable	
toInt

toFloat

Null	
t2 = toInt t1

t2 = toFloat t1

 
