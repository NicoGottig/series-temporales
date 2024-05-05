library(tidyverse)
library(lubridate)
library(showtext)
library(xts)
library(ggseas)
library(tseries)
library(forecast)
library(gridExtra)
library(dynlm)
library(stargazer)
library(lmtest)

# Datos
data <- read_csv("cinear-horasVisitadas.csv")
data$indice_tiempo <- as.Date(data$indice_tiempo, format = "%Y-%m-%d")
colnames(data) <- c("fecha", "visitas")

# Graficamos la serie
data %>% 
  ggplot(aes(y= visitas, x = fecha)) +
  geom_line(col = "darkviolet", linewidth = .7) +
  geom_point(size = 1) +
  theme_light()

# Nos quedamos con 2016 en adelante:
data <- data %>% 
  filter(fecha > "2017-01-01")

# Modelamos con dummy el 2020
data$pandemia <- if_else(data$fecha >= "2020-03-01" , 1, 0)
data$outlier <- if_else(data$fecha == "2020-04-01", 1, 0)

# Analizamos la autocorrelación
g1 = ggAcf(data$visitas, lag.max = 100) + ggtitle("Autocorrelación total") + theme_light()  # MA 
g2 = ggPacf(data$visitas, lag.max = 100) + ggtitle("Autocorrelación parcial") + theme_light() # AR
grid.arrange(g1,g2, nrow = 1)

# Version 1: Auto-Arima
auto.arima(data$visitas, xreg = data$pandemia, trace = T, max.p = 5, max.q = 5)

# Modelado
xreg <- as.matrix(cbind(data$pandemia, data$outlier))

arima1 <- arima(data$visitas, order = c(2,1,2), xreg = xreg)
arima1
summary(arima1)
coeftest(arima1)

# Comparación de series
predicciones_arima <- predict(arima1, newxreg = data$pandemia)
data$predicted <- predicciones_arima$pred

data %>% 
  ggplot() +
  geom_line(aes(x = fecha, y = visitas), col = "blue") +
  geom_line(aes(x = fecha, y = arima1$residuals + 90000), col = "red") + 
  theme_light()

data %>% 
  ggplot() +
  geom_histogram(aes(x = arima1$residuals), fill = "seagreen2", col = "black") +
  theme_bw()

# Análisis de supuestos
data %>% 
  mutate(outlier = if_else(arima1$residuals >= quantile(arima1$residuals, .75) + 1.5 * IQR(arima1$residuals),1,0)) %>% 
  filter(outlier == 1)

# Incorrelación
Box.test(arima1$residuals, type = "Ljung-Box")

# Normalidad
jarque.bera.test(arima1$residuals)

# Análisis de AIC
aic_matrix <- matrix(nrow = 5, ncol = 5)

for (i in 0:5) {
  for (j in 0:5) {
    aic = arima(data$visitas, order = c(i, 1, j), xreg = data$pandemia)$aic
    aic_matrix[i,j] = aic
  }
}

aic_matrix

# Análisis de residuos del modelo arma(1,5)
arma_1_5 <- arima(x = data$visitas, order = c(1,0,4), xreg = data$pandemia)
auto.arima(data$vi)
x11()
tsdiag(arima1)

qqnorm(arma_1_5$residuals)
qqline(arma_1_5$residuals)

# Análisis conjunto de la serie
d2024 <- read.csv("dat-ab-usos-2024.csv")
d2023 <- read.csv("dat-ab-usos-2023.csv")
d2022 <- read.csv("dat-ab-usos-2022.csv")
d2021 <- read.csv("dat-ab-usos-2021.csv")
data <- bind_rows(d2021, d2022, d2023, d2024)

# Filtro casos rosario y agrupo por dia
data <- data %>% 
  filter(MUNICIPIO == "ROSARIO") %>% 
  select(DIA_TRANSPORTE, LINEA, CANTIDAD) %>% 
  group_by(DIA_TRANSPORTE) %>% 
  summarise(cantidad = sum(CANTIDAD)) %>% 
  filter(DIA_TRANSPORTE > "2023-04-01")

data$DIA_TRANSPORTE <- as.Date(data$DIA_TRANSPORTE, format = "%Y-%m-%d")

data$cantidad[73] <- mean(19594, 201567)
data$cantidad[82] <- mean(202954, 184466)
data$cantidad[246] <- mean(157060, 406121)


# Etiqueto finde
dias_semana <- factor(weekdays(data$DIA_TRANSPORTE), levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

data$weekend <- ifelse(dias_semana %in% c("sábado", "domingo", "lunes"), "Finde", "Semanal")

# boxplot
data %>% 
  ggplot() +
  aes(y = cantidad, group = dias_semana, fill = dias_semana) + 
  geom_boxplot()


# Ploteo la serie
data %>% 
  ggplot() +
  aes(x = DIA_TRANSPORTE, y = cantidad) +
  geom_line() +
  geom_point(aes(color = as.factor(weekend))) +
  theme_light() +
  xlab("Fecha") +
  ylab("Transacciones SUBE")

auto.arima(data$cantidad, max.p = 5, max.q = 5, xreg = data$weekend)

arima.pasajes <- arima(data$cantidad, order = c(3,1,5), xreg = data$weekend, seasonal = c(2,1,3))
hist(arima.pasajes$residuals)
jarque.bera.test(arima.pasajes$residuals)

predicciones_arima <- predict(arima.pasajes, newxreg = data$weekend)
data$predicted <- predicciones_arima$pred

data %>% 
  ggplot() +
  geom_line(aes(x = DIA_TRANSPORTE, y = cantidad), col = "blue") +
  geom_line(aes(x = DIA_TRANSPORTE, y = predicted), col = "red") + 
  theme_light()


# Descomposición 
cantidad = ts(data$cantidad, 
             frequency = 7)

desc.aditiva <- decompose(cantidad, type = "additive")
pl1 <- autoplot(desc.aditiva[[3]]) + xlab("Periodo") + ylab("Tendencia") + theme(text = element_text(size = 10))
pl2 <- autoplot(desc.aditiva[[2]]) + xlab("") + ylab("Ciclo") + theme(text = element_text(size = 10))
pl3 <- autoplot(desc.aditiva[[4]]) + xlab("") + ylab("Error") + theme(text = element_text(size = 10))

grid.arrange(pl1, pl2, pl3, nrow = 3)

# Modelo atipicos con dummy:
data$atipico <- if_else(data$cantidad < 15000, 1, 0)

data$dif <- c(NA, diff(diff(data$cantidad)))

# Autocorrelacion etc
g1 = ggAcf(data$dif, ) + ggtitle("Autocorrelación total")  # MA 
g2 = ggPacf(data$dif) + ggtitle("Autocorrelación parcial") # AR
grid.arrange(g1,g2, nrow = 1)
