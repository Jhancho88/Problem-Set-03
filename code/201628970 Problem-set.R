# Taller de R - Problem set 1
#---------------------------------

# Nombre y código
## Jhan Camilo Pulido Rodriguez
## 201628970

# Versión de R
R.version.string
## [1] "R version 4.3.2 (2023-10-31 ucrt)"

# INTRO A R: OPERADORES Y FUNCIONES
# -----------------------------------------------

# Operadores aritméticos:
# Realize una operación que calcule el resultado de 7 elevado a la potencia de 3.
7**3

# Operador lógico:
# Ejemplo de expresión que use un operador lógico para verificar si el valor de 2 es mayor que 10.
2 > 10

# Instalación y llamada de una librería:
# Instala y llama a la librería dplyr en R.
# Puede usar la librería ‘pacman‘ o las funciones install.packages() y require().
install.packages("dplyr")
library(dplyr)

# Obtener información sobre una función:
# Usando la función de ayuda, obtén información sobre sum().
# Muestra un ejemplo de cómo se usa.
help(sum)
# Ejemplo: Suma de los elementos de un vector.
sum(1:3)


# FUNDAMENTOS DE PROGRAMACIÓN: OBJETOS Y WORKSPACE
#----------------------------------------------------------

# Creación y eliminación de objetos en el workspace:
# Crea un objeto llamado mi_numero y asígnale el valor numérico 25
# Crea otro obJeto llamado mi_numero_2 que sea igual al objeto mi_numero a la potencia de 2.
# Ahora elimine el objeto mi_numero del workspace utilizando las funciones vistas en clase.
mi_numero <- 25
mi_numero2 <- (mi_numero)**2
rm(mi_numero)

# Mostrar lista de objetos en el workspace:
ls()

# Guardar el contenido del workspace en un archivo:
# Haga un ejemplo guardando el contenido del workspace en un archivo llamado "backup.RData".
# (**Hint:** Use la función save.image())
getwd() # PAra verificar mi directorio de trabajo actual
setwd("C:/Users/jhanc/OneDrive - Universidad de los andes/CURSOS/2024-1 TallerR/Problem Set/01") # Defino el directorio de trabajo
save.image(file = "backup.RData") # Guardo el contenido del workspace


# ESTRUCTURA DE DATOS EN R: VECTORES Y DATAFRAMES
#-------------------------------------------------------

# Creación de un vector llamado mi_vector.
# Debe contener los nombres de los días de la semana.
mi_vector <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
print(mi_vector)

# Creación de una matriz numérica:
# Defina mi_matriz de 3 filas y 2 columnas con valores numéricos de tu elección.
mi_matriz <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
print(mi_matriz)

# Creación de un dataframe y acceso a columna:
# Cree mi_dataframe con dos columnas: "nombre" y "edad", y agrega algunos datos.
mi_dataframe <- data.frame(nombre = c("Jhan", "Camilo", "Juan"), edad = c(25, 30, 35))
print(mi_dataframe)
mi_dataframe$nombre # Llamo a la columna nombre

# Tibbles: Convierta mi_dataframe en un tibble y muestra cómo se accede a la columna "nombre".
install.packages("tibble") # Instalo tibble
library(tibble) # Cargo tibble

mi_tibble <- as_tibble(mi_dataframe) # convierto el dataframe a tibble
print(mi_tibble)

mi_tibble$nombre # Forma 1: Acceso a la columna "nombre" en un tibble
mi_tibble[["nombre"]] # Forma 2: Acceso a la columna "nombre" en un tibble



# ESTRUCTURA DE DATOS EN R: LISTAS Y MANIPULACIÓN
#-------------------------------------------------------

# Creación de una lista:
# Genere mi_lista que contenga un vector numérico, un dataframe y un valor lógico.
mi_lista <- list(vector_numerico = c(1, 2, 3), dataframe = mi_dataframe, valor_logico = TRUE)
print(mi_lista)

# Acceso a elementos de una lista:
# Muestre un ejemplo cómo acceder al segundo elemento del vector dentro de mi_lista.
mi_lista$vector_numerico[2] # Acceder al segundo elemento del vector dentro de mi_lista

# Combinación de listas:
# Genere dos listas vacias y llamelas lista1 y lista2
lista1 <- list()
lista2 <- list()
# Luego combinelas en una nueva lista llamada lista_combinada.
lista_combinada <- c(lista1, lista2)


# Cree una Lista de listas:
# Cada elemento sea una lista que contenga un vector de nombres y un dataframe.
# Puede usar los objetos mi_dataframe y mi_vector.
lista_de_listas <- list(vector_nombres = mi_vector, dataframe = mi_dataframe)
print(lista_de_listas)

# Otro ejemplo:
lista_de_listas_1 <- list(
  lista_1 = list(vector_nombres = mi_vector, dataframe = mi_dataframe),
  lista_2 = list(vector_nombres = c("Nombre_1", "Nombre_2"), dataframe = data.frame(nombre = c("Persona_1", "Persona_2"), edad = c(21, 22)))
)
print(lista_de_listas_1)
