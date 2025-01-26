# Ver las primeras filas del dataframe 'epa_http'
head(epa_http)

# Verifica los nombres actuales de las columnas
print(colnames(epa_http))

# Cambiar los nombres de las columnas
colnames(epa_http) <- c("IP", "Timestamp", "Request Type", "URL", "Protocol", "Response Code", "Bytes of Reply")

# Verifica los nuevos nombres de las columnas
print(colnames(epa_http))


# Verificar los nombres actuales de las columnas
print(colnames(epa_http))


# Cargar las librerías necesarias
library(dplyr)     # Para manipulación de datos
library(lubridate) # Para trabajar con fechas y horas


############### D E S D E - A Q U Í - E S - E L - E N U N C I A D O - 3 -
############### L  I  M  P  I  E  Z  A  - D E - D  A  T  O  S 


# Convertir los datos al tipo adecuado
epa_http$Timestamp <- ymd_hms(epa_http$Timestamp)                # Convertir a timestamp
epa_http$Response_Code <- as.integer(epa_http$Response_Code)     # Convertir a entero
epa_http$Bytes_of_Reply <- as.numeric(epa_http$Bytes_of_Reply)   # Convertir a numérico

# Limpiar la columna 'Bytes_of_Reply' reemplazando guiones por NA
epa_http$Bytes_of_Reply <- gsub("-", NA, epa_http$Bytes_of_Reply)
epa_http$Bytes_of_Reply <- as.numeric(epa_http$Bytes_of_Reply)

# Remover espacios adicionales en las columnas de tipo cadena de caracteres
epa_http$IP <- trimws(epa_http$IP)
epa_http$Request_Type <- trimws(epa_http$Request_Type)
epa_http$URL <- trimws(epa_http$URL)
epa_http$Protocol <- trimws(epa_http$Protocol)

# Verificar las primeras filas después de la limpieza
head(epa_http)

# Calcular el promedio de la columna 'Bytes_of_Reply'
valor_medio <- mean(epa_http$Bytes_of_Reply, na.rm = TRUE)

# Calcular las dimensiones del dataset
dimensiones <- dim(epa_http)

# Mostrar el resultado del promedio y las dimensiones del dataset
print(paste("Promedio de 'Bytes_of_Reply':", valor_medio))
print(paste("Dimensiones del dataset (Filas, Columnas):", paste(dimensiones, collapse = ", ")))


































## Pregunta 2 

# Filtrar las IPs que contienen ".edu"
ips_edu <- grep("\\.edu", epa_http$IP, value = TRUE)

# Contar el número de IPs educativas
num_ips_edu <- length(ips_edu)

# Mostrar el resultado
print(paste("Número de IPs educativas:", num_ips_edu))


## Pregunta 3


#De todas las peticiones recibidas por el servidor cual es la hora en la que hay mayor volumen de peticiones HTTP de tipo "GET"?

# Filtrar las solicitudes de tipo GET

get_requests = data[data['Request_Type'] == 'GET']

# Extraer la hora de cada petición

get_requests['Hour'] = get_requests['Timestamp'].str.split(':').str[1]


# Calcular la hora con el mayor volumen de peticiones GET

hourly_counts = get_requests['Hour'].value_counts()

peak_hour = hourly_counts.idxmax()

peak_count = hourly_counts.max()

peak_hour, peak_count

El mayor volumen de peticiones HTTP de tipo "GET" es a las 14:00, con 4546 peticiones




## Pregunta 4

library(dplyr)

# Filtrar las peticiones hechas por instituciones educativas
edu_requests <- epa_http %>% filter(grepl("\\.edu$", IP))

# Filtrar las peticiones que corresponden a la descarga de ficheros de texto ".txt"
txt_requests <- edu_requests %>% filter(grepl("\\.txt$", URL))

# Convertir la columna 'Bytes of Reply' a numérico (si no lo está ya)
txt_requests$`Bytes of Reply` <- as.numeric(txt_requests$`Bytes of Reply`)

# Sumar los bytes transmitidos
total_bytes <- sum(txt_requests$`Bytes of Reply`, na.rm = TRUE)

# Mostrar el resultado
print(paste("Total de bytes transmitidos en peticiones .txt por IPs .edu:", total_bytes))


## Pregunta 5

library(dplyr)

# Filtrar las peticiones que buscan la URL "/"
root_url_requests <- epa_http %>% filter(URL == "/")

# Contar el número de peticiones
num_root_url_requests <- nrow(root_url_requests)

# Mostrar el resultado
print(paste("Número de peticiones que buscan la URL '/':", num_root_url_requests))



## Pregunta 6 

library(dplyr)

# Filtrar las peticiones que no tienen el protocolo "HTTP/0.2"
non_http02_requests <- epa_http %>% filter(Protocol != "HTTP/0.2")

# Contar el número de peticiones
num_non_http02_requests <- nrow(non_http02_requests)

# Mostrar el resultado
print(paste("Número de peticiones que no tienen el protocolo 'HTTP/0.2':", num_non_http02_requests))