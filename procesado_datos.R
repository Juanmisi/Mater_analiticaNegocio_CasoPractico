library(readr)
library(dplyr)
library(lubridate)
library(openxlsx)

data <- read_csv("Affinity - State - Daily.csv")

# Convertir las columnas de fecha a una sola columna de fecha
data$date <- as.Date(with(data, paste(year, month, day, sep="-")), "%Y-%m-%d")

# Filtrar los datos a partir del 15 de enero de 2020
data <- data %>% filter(date >= as.Date("2020-01-15"))

# Eliminar los datos del estado 11
data <- data %>% filter(statefips != 11)

# Separar en dos dataframes distintos los valores daily y weekly
data_daily <- data %>% filter(freq == 'd')
data_weekly <- data %>% filter(freq == 'w')

# Convertir las columnas numéricas de gasto a tipo numérico (reemplazar los puntos '.' por NA)
numeric_columns <- names(data_daily)[grepl("spend_", names(data_daily))]
data_daily[numeric_columns] <- lapply(data_daily[numeric_columns], function(x) as.numeric(replace(x, x == ".", NA)))
data_weekly[numeric_columns] <- lapply(data_weekly[numeric_columns], function(x) as.numeric(replace(x, x == ".", NA)))

# Agrupar por estado y semana, y calcular la media de los valores diarios para obtener un solo valor por semana
data_daily_refactored <- data_daily %>%
  group_by(statefips, date = floor_date(date, "week")) %>%
  summarise(across(starts_with("spend_"), mean, na.rm = TRUE), .groups = 'drop')

# Seleccionar las columnas relevantes para ambos datasets
columns_to_keep <- c("statefips", "date", numeric_columns)
data_daily_refactored <- data_daily_refactored %>% select(all_of(columns_to_keep))

data_weekly <- data_weekly %>% select(all_of(columns_to_keep))

# Combinar ambos datasets
combined_data <- bind_rows(data_daily_refactored, data_weekly)

# Mostrar las primeras filas del dataframe combinado
head(combined_data)

# Mostrar las estadisticas básicas del dataframe
summary(combined_data)

# Exportar el dataframe combined_data a un archivo .xlsx
write.xlsx(combined_data, "combined_data.xlsx", rowNames = FALSE)
