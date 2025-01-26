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
