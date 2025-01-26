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

# Paso 5: Visualización de resultados
# Gráfico 1: Número de peticiones por método HTTP
ggplot(metodos_frecuencia, aes(x = method, y = frecuencia, fill = method)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Frecuencia de Métodos HTTP", x = "Método HTTP", y = "Frecuencia")

# Gráfico 2: Número de errores por tipo
ggplot(errores_por_tipo, aes(x = as.factor(status), y = total, fill = as.factor(status))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Errores por Código HTTP", x = "Código HTTP", y = "Frecuencia")

# Gráfico 3: Peticiones a lo largo del tiempo
epa_data$hora <- format(epa_data$timestamp, "%H:%M:%S")
ggplot(epa_data, aes(x = timestamp)) +
  geom_histogram(binwidth = 3600, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Peticiones Servidas a lo Largo del Tiempo", x = "Tiempo", y = "Cantidad")

# Paso 6: Clustering con k-means
# Agregar una columna con la longitud de la URL
epa_data$url_length <- nchar(epa_data$request)

# One-hot encoding de métodos HTTP
epa_data_encoded <- one_hot(as.data.table(epa_data$method))

# Crear clustering_data asegurando que las columnas necesarias están incluidas
clustering_data <- cbind(
  epa_data_encoded,
  url_length = epa_data$url_length, 
  bytes = epa_data$bytes           
)

# Seleccionar únicamente columnas numéricas y manejar NA
clustering_data <- clustering_data %>% 
  select(where(is.numeric)) %>%  
  na.omit()                      

# Verificar si las columnas necesarias están presentes
print(colnames(clustering_data))  

# Aplicar k-means con k = 3 y k = 4
set.seed(123)
kmeans_3 <- kmeans(clustering_data, centers = 3)
kmeans_4 <- kmeans(clustering_data, centers = 4)

# Graficar resultados asegurando que las columnas existen
plot(clustering_data[, c("url_length", "bytes")], 
     col = kmeans_3$cluster,
     main = "Clustering con k = 3",
     xlab = "Longitud de URL",
     ylab = "Bytes")

plot(clustering_data[, c("url_length", "bytes")],  
     col = kmeans_4$cluster,
     main = "Clustering con k = 4",
     xlab = "Longitud de URL",
     ylab = "Bytes")
