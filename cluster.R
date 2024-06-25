library(openxlsx)
library(lubridate)
library(dtwclust)
library(dplyr)

data <- read.xlsx("combined_data.xlsx")
data_states <- read.csv("GeoIDs - State.csv")


# Convertir las columnas de fecha a una sola columna de fecha
data$date <- as.Date(data$date, origin = "1899-12-30")

# Reemplazamos na con la media de la columna
data[is.na(data)] <- sapply(data, function(x) mean(x, na.rm = TRUE))

# Gráfico de correlaciones
numeric_columns <- names(data)[grepl("spend_", names(data))]

mv <- list()
mv_states <- list()
for (i in 1:nrow(data_states)){
  element <- data_states %>% select(statefips) %>% slice(i)
  mv[[length(mv)+1]] = data.matrix(data %>% filter(statefips == element$statefips) %>% select(numeric_columns))
  state <- data_states %>% select(statename) %>% slice(i)
  mv_states[[length(mv_states) + 1]] = state$statename
}

#Quito elementos nulos
condition <- lapply(mv, length) == 0
mv[condition] <- NULL
mv_states[condition] <- NULL

# Using GAK distance
mvc <- tsclust(mv, k = 10L, distance = "gak", seed = 390,
               args = tsclust_args(dist = list(sigma = 100)))

# Note how the variables of each series are appended one after the other in the plot
plot(mvc)

estados <- c()
clusteres <- c()
#Creamos los nombres y clusteres
for (i in 1:length(mv_states)){
  estados <- c(estados, mv_states[i][[1]])
  clusteres <- c(clusteres, mvc@cluster[i])
}
resumen <- data.frame(estados = estados, clusteres = clusteres)

write.csv(resumen, file = 'resume.csv')
