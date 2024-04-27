# Taller de R - Problem set 3
#---------------------------------

# Nombre y código
## Jhan Camilo Pulido Rodriguez
## 201628970

# Versión de R
R.version.string
## [1] "R version 4.3.2 (2023-10-31 ucrt)"

# Cargar librerías necesarias usando pacman
if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load(rio, # permite leer/escribir archivos desde diferentes formatos
       data.table,
       tidyverse, # colección de paquetes para ciencia de datos (incluye dplyr)
       skimr, # describe un conjunto de datos
       dplyr
       )

# Obtener y establecer el directorio de trabajo
getwd()
setwd("C:/Users/jhanc/OneDrive - Universidad de los andes/CURSOS/2024-1 TallerR/Problem Set 03")

### 1. BUCLE

## 1.1. Lista de archivos INPUT

# Directorio de los archivos de entrada
input_dir <- "input"

# Obtener los nombres de los archivos en el directorio de entrada, de forma recursiva
file_names <- list.files(path = input_dir, full.names = TRUE, recursive = TRUE)

# Imprimir los nombres de los archivos con rutas completas
print(file_names)


## 1.2. Importar archivos
# Use el objeto creado en el punto anterior como insumo de una función
# Debe permitir importar los archivos de Fuerza de trabajo, No ocupados y Ocupados para todos los meses.
# Puede crear una función que importe un archivo y combinarla con la función lapply o crear un bucle y almacenar los dataframes en un objeto tipo lista.

# Función para cargar archivos RDS
load_rds_file <- function(file_name) {
  # Leer el archivo RDS
  data <- readRDS(file_name)
  return(data)
}

# Filtrar nombres de archivos específicos de interés
target_files <- file_names[grepl("Fuerza de trabajo.rds|No ocupados.rds|Ocupados.rds", file_names)]

# Aplicar la función load_rds_file a cada archivo filtrado y almacenar en una lista
data_list <- lapply(target_files, load_rds_file)

# Inspeccionar uno de los dataframes cargados, por ejemplo el primero
print(data_list[[1]])



## 1.3. Importar archivos
# Combina todos los data.frame que importaste en el punto anterior en tres diferentes data.frames
# Es decir, use la función rbind.list para crear un dataframe con los archivos de Fuerza de trabajo, otro con los archivos de No ocupados y otro con los archivos de Ocupados.

# Crear listas separadas por tipo de archivo
fuerza_trabajo_list <- list()
no_ocupados_list <- list()
ocupados_list <- list()

# Llenar las listas con los dataframes correspondientes
for (file_name in target_files) {
  if (grepl("Fuerza de trabajo.rds", file_name)) {
    fuerza_trabajo_list[[length(fuerza_trabajo_list) + 1]] <- load_rds_file(file_name)
  } else if (grepl("No ocupados.rds", file_name)) {
    no_ocupados_list[[length(no_ocupados_list) + 1]] <- load_rds_file(file_name)
  } else if (grepl("Ocupados.rds", file_name)) {
    ocupados_list[[length(ocupados_list) + 1]] <- load_rds_file(file_name)
  }
}

# Combinar cada lista en un único dataframe usando rbindlist
fuerza_trabajo_df <- rbindlist(fuerza_trabajo_list)
no_ocupados_df <- rbindlist(no_ocupados_list, fill = TRUE)  # Usar fill=TRUE para manejar columnas faltantes
ocupados_df <- rbindlist(ocupados_list)

# Opcional: Verificar las dimensiones de los dataframes combinados
print(dim(fuerza_trabajo_df))
print(dim(no_ocupados_df))
print(dim(ocupados_df))





### 2. PREPARACION

## 2.1. Creacion de bases de datos

# Asignar dataframes a nuevos nombres de objetos
# library(data.table)

# Guardar los dataframes en la subcarpeta 'input' como archivos RDS
saveRDS(fuerza_trabajo_df, "input/fuerza_de_trabajo.rds")
saveRDS(no_ocupados_df, "input/no_ocupados.rds")
saveRDS(ocupados_df, "input/ocupados.rds")


# Cargar los datos de la base "fuerza_de_trabajo"
fuerza_de_trabajo <- readRDS("input/fuerza_de_trabajo.rds")
head(x = fuerza_de_trabajo, n = 5)

# Calcular la suma de individuos en la fuerza laboral y PET por mes, usando FEX_C18
suma_fuerza_laboral <- fuerza_de_trabajo %>%
  filter(FT == 1 | PET == 1) %>%
  group_by(Month) %>%
  summarise(Total_FEX = sum(FEX_C18, na.rm = TRUE))

# Ver los resultados
print(suma_fuerza_laboral)








fuerza_de_trabajo <- fuerza_trabajo_df


no_ocupados <- no_ocupados_df
# Para ver las primeras 5 filas de la base modificada
head(x = no_ocupados, n = 5)

ocupados <- ocupados_df
# Para ver las primeras 5 filas de la base modificada
head(x = ocupados, n = 5)

# Sumar individuos en fuerza_de_trabajo
resumen_fuerza_trabajo <- fuerza_de_trabajo %>%
  filter(ft == 1 | pet == 1) %>%
  group_by(mes) %>%
  summarise(Total = sum(factor_expansion))

# Mostrar el resultado
print(resumen_fuerza_trabajo)



# Sumar individuos en no_ocupados
resumen_no_ocupados <- no_ocupados %>%
  filter(dsi == 1) %>%
  group_by(mes) %>%
  summarise(Total = sum(factor_expansion))

# Mostrar el resultado
print(resumen_no_ocupados)



# Sumar individuos en ocupados
resumen_ocupados <- ocupados %>%
  filter(ft == 1) %>%
  group_by(mes) %>%
  summarise(Total = sum(factor_expansion))

# Mostrar el resultado
print(resumen_ocupados)





























identification <- import("input/Módulo de identificación.dta")
location <- import("input/Módulo de sitio o ubicación.dta")

## 1.2. Exportar
export(x=identification, file="output/identification.rds")
export(x=location, file="output/location.rds")

## Explorar base de datos
head(x = identification , n = 5) # Primeras 5 obs de identification
head(x = location , n = 5) # Primeras 5 obs de location

str(object = identification) ## estructura de "identification"
str(object = location) ## estructura de "location"

glimpse(x = identification) ## estructura de`"identification"
glimpse(x = location) ## estructura de`"identification"

skim(data = identification)
skim(data = location)

summary(identification)  ## describir base de datos "identification"
summary(location)  ## describir base de datos "location"



### 2. GENERAR VARIABLES

## 2.1. Añade variable "business_type" a la base "identification"
identification <- mutate(identification, 
                         bussiness_type = case_when(
                           GRUPOS4 == "01" ~ "Agricultura",
                           GRUPOS4 == "02" ~ "Industria manufacturera",
                           GRUPOS4 == "03" ~ "Comercio",
                           GRUPOS4 == "04" ~ "Servicios",
                         ))

# Para ver las primeras 5 filas de la base modificada
head(x = identification, n = 5)


## 2.2. Crear variable grupo_etario
# Debe dividir a los propietarios de micronegocios en 4 grupos etarios.
# Rangos de edades seleccionados deben ser justificados.
identification <- mutate(identification,
                         grupo_etario = case_when(
                           P241 >= 18 & P241 <= 34 ~ "Adultos Jóvenes",
                           P241 >= 35 & P241 <= 45 ~ "Adultos A",
                           P241 >= 46 & P241 <= 56 ~ "Adultos B",
                           P241 >= 57 ~ "Adultos mayores"
                         ))

# Para ver las primeras 5 filas de la base modificada
head(x = identification, n = 5)

# Rango 1: Del valor mínimo al primer cuartil
# Rango 2: Del primer cuartil a la mediana
# Rango 3: Entre la mediana y el tercer cuartil
# Rango 4: Del tercer cuartil hasta el valor máximo



## 2.3. Sobre el objeto "location", genere la variable "ambulante"
# Sera igual a 1 si la variable P3053 es igual a 3, 4 o 5.
location <- mutate(location,
                   ambulante = case_when(
                     P3053 %in% c(3, 4, 5) ~ 1,
                     TRUE ~ 0 # Asigna 0 a cualquier otro valor de P3053
                   ))

# Para ver las primeras 5 filas de la base modificada
head(x = location, n = 5)



### 3. ELIMINAR FILAS/COLUMNAS DE UN CONJUNTO DE DATOS
# 3.1. Almacene en un objeto llamado identification_sub las variables:
# DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, grupo_etario, ambulante, COD_DEPTO y F_EXP.
names(identification) # Para "identification"
names(location) # Para "location"

# Realizamos la unión de los dataframes basándonos en las columnas comunes.
identification_full <- left_join(identification, location, 
                                 by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"))

# Ahora seleccionamos solo las columnas especificadas para crear 'identification_sub'.
identification_sub <- select(identification_full, 
                             DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, 
                             grupo_etario, ambulante, COD_DEPTO.x, F_EXP.x)

# Verificamos primeras filas del nuevo dataframe.
head(identification_sub)



## 3.2. Del objeto location seleccione solo las variables:
# DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ambulante P3054, P469, COD_DEPTO, F_EXP
# Guárdelo en nuevo objeto llamado location_sub.
location_sub <- select(location, 
                       DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, 
                       ambulante, P3054, P469, COD_DEPTO, F_EXP) # Seleccion de variables en location

head(location_sub)




### 4. COMBINAR BASES DE DATOS

# 4.1. Use las variables DIRECTORIO, SECUENCIA_P y SECUENCIA_ENCUESTA
# para unir en una única base de datos, los objetos location_sub y identification_sub.

# Uniendo "location_sub" y "identification_sub" en una única base de datos
combined_data <- full_join(location_sub, identification_sub, by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"))



### 5. DESCRIPTIVAS
# 5.1. Usando funciones como skim o summary,
# cree breves estadísticas descriptivas de la base de datos creada previamente.
# (HINT: Observaciones en NA, conteo de variables únicas)
summary(combined_data)
skim(combined_data)



# 5.2. Use las funciones group_by y summarise para extraer variables descriptivas, como:
# cantidad de asociados por departamento, grupo etario, entre otros.
# Además, cree un pequeño párrafo con los hallazgos que encuentre.
library(dplyr)

# Cantidad de asociados por departamento
asociados_por_departamento <- combined_data %>%
  group_by(COD_DEPTO) %>%
  summarise(cantidad_asociados = n())

asociados_por_departamento


# Cantidad de asociados por grupo etario
asociados_por_grupo_etario <- combined_data %>%
  group_by(grupo_etario) %>%
  summarise(cantidad_asociados = n())

asociados_por_grupo_etario


# Analisis

# La variación en el número de asociados por departamento sugiere diferencias regionales en participación, con los departamentos 13 y 08 liderando en número de asociados. Esto podría reflejar factores como mayor población y actividad económica, indicando también oportunidades de crecimiento en regiones con menos asociados. Estos hallazgos podrían ayudar a la organización a optimizar sus estrategias de alcance y engagement.
# La distribución de asociados por grupo etario es relativamente equilibrada, destacándose ligeramente los Adultos Jóvenes. Esto muestra éxito en atraer una amplia diversidad de edades, aunque el interés predominante de los Adultos Jóvenes sugiere que las ofertas de la organización podrían estar más alineadas con sus necesidades o intereses. Analizar más detalladamente las razones detrás de estas distribuciones podría ayudar a mejorar la diversidad etaria y el atractivo de la organización.







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
