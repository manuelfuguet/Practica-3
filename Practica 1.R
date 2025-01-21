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


epa_http$`Bytes of Reply` <- gsub("-", NA, epa_http$`Bytes of Reply`)

# Convertir la columna 'Bytes of Reply' a numérico
epa_http$`Bytes of Reply` <- as.numeric(epa_http$`Bytes of Reply`)


valor_medio <- mean(epa_http$`Bytes of Reply`, na.rm = TRUE)


# Calcular las dimensiones del dataset
dimensiones <- dim(epa_http)

# Mostrar el resultado del promedio y las dimensiones del dataset

print(paste("Promedio de 'Bytes of Reply':", valor_medio))
print(paste("Dimensiones del dataset (Filas, Columnas):", paste(dimensiones, collapse = ", ")))


## Pregunta 2 

# Filtrar las IPs que contienen ".edu"
ips_edu <- grep("\\.edu", epa_http$IP, value = TRUE)

# Contar el número de IPs educativas
num_ips_edu <- length(ips_edu)

# Mostrar el resultado
print(paste("Número de IPs educativas:", num_ips_edu))



## Pregunta 3







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