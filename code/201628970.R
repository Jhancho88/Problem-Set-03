## PROBLEM SET 2
# Jhan Camilo Pulido Rodriguez

# Versión de R
R.version.string
#"R version 4.3.2 (2023-10-31 ucrt)"

library(pacman)
library(dplyr)

p_load(rio, # permite leer/escribir archivos desde diferentes formatos
       data.table,
       tidyverse,
       skimr, # describe un conjunto de datos
       dplyr
       )

getwd()
setwd("C:/Users/jhanc/OneDrive - Universidad de los andes/CURSOS/2024-1 TallerR/Github/problem_sets/pset-2")

### 1. IMPORTAR/EXPORTAR BASES DE DATOS

## 1.1. Importar
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
