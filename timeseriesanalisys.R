library(openxlsx)
library(lubridate)
library(corrplot)
library(RColorBrewer)
library(forecast)

data <- read.xlsx("combined_data.xlsx")

# Convertir las columnas de fecha a una sola columna de fecha
data$date <- as.Date(data$date, origin = "1899-12-30")

# Reemplazamos na con la media de la columna
data[is.na(data)] <- sapply(data, function(x) mean(x, na.rm = TRUE))

#Agregamos los datos de todos los estados
data <- aggregate(x = data, by = list(data$date), FUN = mean)

# Gráfico de correlaciones
numeric_columns <- names(data)[grepl("spend_", names(data))]
Correlation_matrix <-cor(data[numeric_columns])
corrplot(Correlation_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#Ploteo de las series a analizar
ggplot(data, aes(date)) +       # Create ggplot2 plot
  geom_line(aes(y = spend_durables, color = "spend_durables", group=1, )) +
  geom_line(aes(y = spend_nondurables, color = "spend_nondurables", group=1)) +
  geom_line(aes(y = spend_remoteservices, color = "spend_remoteservices", group=1)) +
  geom_line(aes(y = spend_inperson, color = "spend_inperson", group=1)) +
  labs(y = "Value", x= "Date", lintype = "Legend")


#Dividimos los datos quitando el último año para ponerlo como test
data_train <- data[data$date < "2024-01-01",]
data_test <- data[data$date >= "2024-01-01",]

###Estudio individual de cada serie
  #spend_durables

#Calculamos las diferencias entre días
dif_spend_durables <- diff(ts(data$spend_durables))

#Ploteo de las diferencias
plot(dif_spend_durables, type ="l", main = "Diferencias spend_durables", col = "red")

#Variaciones de las diferencias con función de densidad
hist(dif_spend_durables, probability = TRUE, breaks = 30 , ylim = c(0, 20) , main = "spend_durables Distribución", col = "red") #con breaks definimos los intervalos
lines(density(dif_spend_durables), lwd = 3)


#DESCOMPOSICION SERIES
#Método LOESS
flour.spend_durables <- log1p(ts(data$spend_durables, frequency = 52))
flour.stl.spend_durables <- stl(flour.spend_durables, s.window = "period")
plot(flour.stl.spend_durables)

#Método medias móviles
flour.dec.spend_durables <- decompose(flour.spend_durables)
plot(flour.dec.spend_durables)

#Suavizado y predicción por el método de HOLT-WINTERS
spend_durables.hw <- HoltWinters(ts(data_train$spend_durables, frequency = 52))
head(spend_durables.hw)
plot(spend_durables.hw, col = "blue", col.predicted = "red")
spend_durables.fore <- forecast(spend_durables.hw, h=52)
plot(spend_durables.fore)
accuracy(spend_durables.fore, data_test$spend_durables)


#spend_nondurables

#Calculamos las diferencias entre días
dif_spend_nondurables <- diff(ts(data$spend_nondurables))

#Ploteo de las diferencias
plot(dif_spend_nondurables, type ="l", main = "Diferencias spend_nondurables", col = "red")

#Variaciones de las diferencias con función de densidad
hist(dif_spend_nondurables, probability = TRUE, breaks = 30 , ylim = c(0, 20) , main = "spend_nondurables Distribución", col = "red") #con breaks definimos los intervalos
lines(density(dif_spend_nondurables), lwd = 3)


#DESCOMPOSICION SERIES
#Método LOESS
flour.spend_nondurables <- log1p(ts(data$spend_nondurables, frequency = 52))
flour.stl.spend_nondurables <- stl(flour.spend_nondurables, s.window = "period")
plot(flour.stl.spend_nondurables)

#Método medias móviles
flour.dec.spend_nondurables <- decompose(flour.spend_nondurables)
plot(flour.dec.spend_nondurables)

#Suavizado y predicción por el método de HOLT-WINTERS
spend_nondurables.hw <- HoltWinters(ts(data_train$spend_nondurables, frequency = 52))
head(spend_nondurables.hw)
plot(spend_nondurables.hw, col = "blue", col.predicted = "red")
spend_nondurables.fore <- forecast(spend_nondurables.hw, h=52)
plot(spend_nondurables.fore)
accuracy(spend_nondurables.fore, data_test$spend_nondurables)

#spend_remoteservices

#Calculamos las diferencias entre días
dif_spend_remoteservices <- diff(ts(data$spend_remoteservices))

#Ploteo de las diferencias
plot(dif_spend_remoteservices, type ="l", main = "Diferencias spend_remoteservices", col = "red")

#Variaciones de las diferencias con función de densidad
hist(dif_spend_remoteservices, probability = TRUE, breaks = 30 , ylim = c(0, 20) , main = "spend_remoteservices Distribución", col = "red") #con breaks definimos los intervalos
lines(density(dif_spend_remoteservices), lwd = 3)


#DESCOMPOSICION SERIES
#Método LOESS
flour.spend_remoteservices <- log1p(ts(data$spend_remoteservices, frequency = 52))
flour.stl.spend_remoteservices <- stl(flour.spend_remoteservices, s.window = "period")
plot(flour.stl.spend_remoteservices)

#Método medias móviles
flour.dec.spend_remoteservices <- decompose(flour.spend_remoteservices)
plot(flour.dec.spend_remoteservices)

#Suavizado y predicción por el método de HOLT-WINTERS
spend_remoteservices.hw <- HoltWinters(ts(data_train$spend_remoteservices, frequency = 52))
head(spend_remoteservices.hw)
plot(spend_remoteservices.hw, col = "blue", col.predicted = "red")
spend_remoteservices.fore <- forecast(spend_remoteservices.hw, h=52)
plot(spend_remoteservices.fore)
accuracy(spend_remoteservices.fore, data_test$spend_remoteservices)


#spend_inperson

#Calculamos las diferencias entre días
dif_spend_inperson <- diff(ts(data$spend_inperson))

#Ploteo de las diferencias
plot(dif_spend_inperson, type ="l", main = "Diferencias spend_inperson", col = "red")

#Variaciones de las diferencias con función de densidad
hist(dif_spend_inperson, probability = TRUE, breaks = 30 , ylim = c(0, 20) , main = "spend_inperson Distribución", col = "red") #con breaks definimos los intervalos
lines(density(dif_spend_inperson), lwd = 3)


#DESCOMPOSICION SERIES
#Método LOESS
flour.spend_inperson <- log1p(ts(data$spend_inperson, frequency = 52))
flour.stl.spend_inperson <- stl(flour.spend_inperson, s.window = "period")
plot(flour.stl.spend_inperson)

#Método medias móviles
flour.dec.spend_inperson <- decompose(flour.spend_inperson)
plot(flour.dec.spend_inperson)

#Suavizado y predicción por el método de HOLT-WINTERS
spend_inperson.hw <- HoltWinters(ts(data_train$spend_inperson, frequency = 52))
head(spend_inperson.hw)
plot(spend_inperson.hw, col = "blue", col.predicted = "red")
spend_inperson.fore <- forecast(spend_inperson.hw, h=52)
plot(spend_inperson.fore)
accuracy(spend_inperson.fore, data_test$spend_inperson)
