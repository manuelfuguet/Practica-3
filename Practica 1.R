library(ggplot2)
library(dplyr)
library(data.table)
library(mltools)

# Paso 1: Cargar y explorar los datos

# Cargar los datos desde el archivo CSV
file_path <- "~/Desktop/epa-http.csv"
epa_data <- read.csv(file_path, header = FALSE, sep = " ", stringsAsFactors = FALSE)

# Renombrar columnas
colnames(epa_data) <- c("host", "timestamp", "request", "status", "bytes")

# Transformar tipos de datos
epa_data$timestamp <- as.POSIXct(epa_data$timestamp, format = "[%d:%H:%M:%S]", tz = "UTC")
epa_data$status <- as.numeric(epa_data$status)
epa_data$bytes <- as.numeric(epa_data$bytes)

# Manejar valores faltantes
epa_data$bytes[is.na(epa_data$bytes)] <- 0

# Filtrar códigos HTTP válidos
valid_status_codes <- c(200, 201, 202, 204, 301, 302, 304, 400, 401, 403, 404, 500, 501)
epa_data <- epa_data %>% filter(status %in% valid_status_codes)


# Paso 2: Descripción de los datos

cat("Descripción de los datos:\n")
cat("Número de registros:", nrow(epa_data), "\n")
cat("Columnas:\n")
print(colnames(epa_data))
summary(epa_data)

# Paso 3:Explorar datos únicos y errores

usuarios_error <- epa_data %>%
  mutate(error = status >= 400) %>%
  group_by(host, error) %>%
  summarize(total_requests = n()) %>%
  ungroup()

errores_por_tipo <- epa_data %>%
  filter(status >= 400) %>%
  group_by(status) %>%
  summarize(total = n())

cat("Resumen de errores:\n")
print(errores_por_tipo)



# Paso 4:Tipos de peticiones HTTP
# Extraer el método HTTP del campo "request"

epa_data$method <- sapply(strsplit(epa_data$request, " "), [, 1)

# Contar frecuencia de cada método
metodos_frecuencia <- epa_data %>%
  group_by(method) %>%
  summarize(frecuencia = n()) %>%
  arrange(desc(frecuencia))

cat("Frecuencia de métodos HTTP:\n")
print(metodos_frecuencia)

# Frecuencia para recursos de tipo imagen
imagen_frecuencia <- epa_data %>%
  filter(grepl("\\.(jpg|png|gif|ico|bmp)$", request, ignore.case = TRUE)) %>%
  group_by(method) %>%
  summarize(frecuencia = n())

cat("Frecuencia de métodos HTTP para recursos de imagen:\n")
print(imagen_frecuencia)
