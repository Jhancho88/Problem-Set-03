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
       dplyr,
       ggplot2
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

## PROCESAMIENTO FUERZA LABORAL
# Cargar los datos de la base "fuerza_de_trabajo"
fuerza_de_trabajo <- readRDS("input/fuerza_de_trabajo.rds")

head(x = fuerza_de_trabajo, n = 5)
print(colnames(fuerza_de_trabajo)) # Verificar las columnas disponibles en el dataframe

# Calcular la suma de individuos en la fuerza laboral y PET por mes
suma_fuerza_laboral <- fuerza_de_trabajo %>%
  filter(FT == 1 | PET == 1) %>%
  group_by(MES) %>%
  summarise(Total_FEX = sum(FEX_C18, na.rm = TRUE))

# Ver los resultados
print(suma_fuerza_laboral)



# PROCESAMIENTO OCUPOADOS
# Cargar los datos de la base "ocupados"
ocupados <- readRDS("input/ocupados.rds")
head(x = ocupados, n = 5)
print(colnames(ocupados)) # Verificar las columnas disponibles en el dataframe

# Calcular la suma de individuos empleados por mes
suma_ocupados <- ocupados %>%
  filter(FT == 1) %>%
  group_by(MES) %>%
  summarise(Total_FEX = sum(FEX_C18, na.rm = TRUE))

# Ver los resultados
print(suma_ocupados)


# PROCESAMIENTO NO OCUPADOS
# Cargar los datos de la base "no_ocupados"
no_ocupados <- readRDS("input/no_ocupados.rds")
head(x = no_ocupados, n = 5)
print(colnames(no_ocupados)) # Verificar las columnas disponibles en el dataframe

# Calcular la suma de individuos NO OCUPADOS por mes, usando FEX_C18
suma_no_ocupados <- no_ocupados %>%
  filter(DSI == 1) %>%
  group_by(MES) %>%
  summarise(Total_FEX = sum(FEX_C18, na.rm = TRUE))

# Ver los resultados
print(suma_no_ocupados)




## 2.2. Colapsar datos a nivel mensual
# Unifica todas las bases en una única base llamada Output
# Debe contener al menos cinco columnas: Población en edad de trabajar, fuerza laboral, ocupados, desempleados y el mes correspondiente.

# Cargar las bases de datos
fuerza_de_trabajo <- readRDS("input/fuerza_de_trabajo.rds")
ocupados <- readRDS("input/ocupados.rds")
no_ocupados <- readRDS("input/no_ocupados.rds")

# Seleccionar las variables especificadas de cada base de datos
fuerza_de_trabajo <- fuerza_de_trabajo[, c("PET", "FT", "MES")]
ocupados <- ocupados[, c("FT", "MES")]
no_ocupados <- no_ocupados[, c("DSI", "MES")]

# Convertir data.frames a data.tables
# Tuve que ejecutar esta transformacion porque las columnas son distintas en cada base y solo asi podia manipularlas mas facilmente
# aplicando un N/A en los registros en los que fuera necesario. realice un append.
fuerza_de_trabajo_dt <- as.data.table(fuerza_de_trabajo)
ocupados_dt <- as.data.table(ocupados)
no_ocupados_dt <- as.data.table(no_ocupados)

# Unificar todas las bases en una única base usando rbindlist con fill = TRUE
output_dt <- rbindlist(list(fuerza_de_trabajo_dt, ocupados_dt, no_ocupados_dt), fill = TRUE)

# Verificar la estructura del dataframe combinado
print(str(output_dt))

# Guardar la base de datos combinada en la subcarpeta "output"
saveRDS(output_dt, "output/output.rds")



## 2.3. Tasas de desempleo y ocupacion
# Divida el número de individuos desempleados por la fuerza laboral para obtener la tasa de desempleo
# Divida el número de ocupados por la población en edad de trabajar para obtener la tasa de ocupación.

output <- readRDS("output/output.rds") # Cargar la base de datos combinada
output <- as.data.table(output) # Convertir a data.table
print(str(output)) # Verificacion

# Formateo de numero en las variables requeridas (y tratamiento a N/A)
output[, c("FT", "PET", "DSI") := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = c("FT", "PET", "DSI")]

# Calcular la suma de no ocupados, ocupados y la fuerza laboral por MES si está disponible
aggregated_data <- output[, .(Total_DSI = sum(DSI),
                              Total_FT = sum(FT),
                              Total_PET = sum(PET)), by = MES]

# Calcular la tasa de desempleo y la tasa de ocupación
aggregated_data[, `:=` (Tasa_Desempleo = Total_DSI / Total_FT,
                        Tasa_Ocupacion = Total_FT / Total_PET)]

# Mostrar los resultados
print(aggregated_data)




### GGPLOT2
# Grafique las tasas de desempleo y ocupación para cada mes utilizando la función geom_line.
# Realice un pivot wider a las tasas de modo que los valores estén en una sola columna.

# Preparacion de datos para el grafico
aggregated_data_long <- aggregated_data %>%
  pivot_longer(cols = c("Tasa_Desempleo", "Tasa_Ocupacion"), 
               names_to = "Tipo_Tasa", 
               values_to = "Valor")

# Verificar la estructura de los datos transformados
print(aggregated_data_long)


# Grafica
ggplot(aggregated_data_long, aes(x = MES, y = Valor, color = Tipo_Tasa, group = Tipo_Tasa)) +
  geom_line() +
  labs(title = "Tasas de Desempleo y Ocupación por Mes",
       x = "Mes",
       y = "Valor de la Tasa",
       color = "Tipo de Tasa") +
  theme_minimal()


# Otro intento para mejorar legibilidad
# Aumentar la base size para todo el texto en el gráfico
base_size <- 14

# Crear el gráfico con mejoras visuales
p <- ggplot(aggregated_data_long, aes(x = MES, y = Valor, color = Tipo_Tasa, group = Tipo_Tasa)) +
  geom_line(size = 1) +  # Hacer las líneas un poco más gruesas
  scale_x_discrete(name = "Mes") +
  scale_y_continuous(name = "Valor de la Tasa", labels = scales::percent) +
  scale_color_manual(values = c("Tasa_Desempleo" = "red", "Tasa_Ocupacion" = "blue")) +
  labs(title = "Tasas de Desempleo y Ocupación por Mes",
       subtitle = "Datos de la Gran Encuesta Integrada de Hogares (GEIH) 2023",
       caption = "Fuente: DANE") +
  theme_minimal(base_size = base_size) +
  theme(
    text = element_text(size = base_size), # Aumentar el tamaño de la fuente
    plot.title = element_text(face = "bold", size = base_size * 1.2), # Título en negrita y más grande
    plot.subtitle = element_text(size = base_size), # Subtítulo
    plot.caption = element_text(size = base_size * 0.8), # Texto de la fuente más pequeño
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar las etiquetas del eje x
    legend.title = element_text(size = base_size), # Tamaño de fuente para el título de la leyenda
    legend.text = element_text(size = base_size) # Tamaño de fuente para el texto de la leyenda
  )

# Visualizar el gráfico
print(p)

# Guardar el gráfico
ggsave("output/tasa_desempleo_ocupacion.png", p, width = 12, height = 8, dpi = 300)

