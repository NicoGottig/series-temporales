library(tidyverse)
library(forecast)
library(tseries)
library(gridExtra)
library(lmtest)
library(reshape2)
library(showtext)
library(extrafont)
library(vars)
library(zoo)

# Var
df <- readxl::read_excel("tp final/datos/bdfinal.xlsx")
df$inf_log <- log(df$ipc_var)
df$fecha <- as.Date(df$fecha)

# Formato ts
series <- df %>% 
  dplyr::select(-fecha) %>% 
  ts(start = c(2011,01), frequency = 12)

# Seleccion de variables
variables <- c("inf_log", "a3500_var", "badlar_mensual")

# Estacionariedad
library(tseries)

# Primeras diferencias
series_dif <- diff(series)

for (i in 1:ncol(series)) {
  result <- adf.test(series[,i])[[4]]
  print("##########")
  print(paste("VARIABLE", colnames(series)[i]))
  print(paste("p-valor:", result))
}

# En primeras diferencias son estacionarias 
n_vars <- NCOL(series)
matriz_granger <- matrix(NA, ncol = n_vars, nrow = n_vars,
                         dimnames = list(colnames(series), colnames(series)))

# Iterar sobre todas las combinaciones posibles de variables
for (i in 1:n_vars) {
  for (j in 1:n_vars) {
    if (i != j) {  # Evitar comparar la misma serie consigo misma
      
      # Calcular la causalidad de Granger entre las series i y j
      granger_result <- grangertest(series[,i] ~ series[,j])[2,4]
      
      # Almacenar el p-valor en la matriz
      matriz_granger[i, j] <- granger_result
      
    }else{
      
      matriz_granger[i,j] <- 0
      
    }
  }
}

melted_matriz_granger <- melt(matriz_granger)

ggplot(data = melted_matriz_granger, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "#4c7b8f") +
  labs(title = "P-valores de la causalidad de Granger",
       x = "Variable Y",
       y = "Variable X",
       fill = "P-Valor") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20),
        legend.position = "none") +
  theme_bw() 

x11()
m_granger1

# Pairs
x11()
GGally::ggpairs(as.data.frame(series[,variables]), 
                title = "") + theme_bw()

# exogenas
semestre_2023 <- if_else(year(df$fecha[-1]) == 2023 & month(df$fecha[-1]) >= 6, 1, 0)
outlier_1416 <- if_else(year(df$fecha[-1]) %in% c(2014,2016), 1, 0)
outlier_2023 <- if_else(df$fecha[-1] %in% c(as.Date("2023-12-01"), as.Date("2024-01-01")), 1, 0)

# Seleccion de var
vars::VARselect(var_df,
                exogen = cbind(semestre_2023, outlier_2023))

# Modelo
var_1 <- vars::VAR(var_df,
                   exogen = cbind(semestre_2023, outlier_2023), type = "both",
                   p = 7)
summary(var_1)

# Supuestos

# Hipótesis nula de autocorrelacion (rechazar)
seriala <- serial.test(x = var_1, 
                       lags.pt = 11, 
                       type = "PT.asymptotic")
seriala

# Hipótesis nula de normalidad (no rechazar)
normalidad <- normality.test(var_1)
normalidad

# Hipótesis nula de homoscedasticidad (no rechazar)
arch1 <- arch.test(var_1, lags.multi=11)
arch1

# Análisis de residuos
hist(var_1$varresult$ipc_log_ipc$residuals, breaks = 11)

# RSME
x11()
predict(var_1, n.ahead = 12, dumvar = cbind(rep(0,12),rep(0,12))) %>% plot()


# Modelo y pronostico con train y test ####

# Train y test
training <- window(diff(series),
                   start = c(2011,02), 
                   end = c(2023,03))

testing <- window(diff(series), 
                  start = c(2023,04), 
                  end = c(2024,03))

variables <- c("inf_log", "a3500_var", "badlar_mensual")
var_df <- training[, colnames(training) %in% variables]
vars::VARselect(var_df, type = "both")

# modelo
t_var_1 <- VAR(var_df, p = 8)
t_var_1 %>% summary()

# forecast y validación cruzada
var_forecast <- predict(t_var_1, n.ahead = 15, dumvar = as.matrix(rep(0,15)))
ipc_forecast <- var_forecast$fcst$inf_log

# RMSE
e <- data.frame(predicho = ipc_forecast[,1],
                real = c(testing[,"inf_log"],0,0,0) %>% c())

sqrt(mean((exp(e$real) - exp(e$predicho))^2))


# agrego fecha
e$fecha <- seq(as.Date("2023-04-01"), as.Date("2024-06-01"), by = "month")

# agrego ipc real
ipc_real_p_forecast <- df[148:159, "inf_log"]
e$inf_log <- c(ipc_real_p_forecast$inf_log, NA, NA, NA)

# forecastear ipc
e$ipc_log_predicho <- lag(e$inf_log) + e$predicho
e$ipc_log_predicho[1] <- 2.041+(-0.18228)
e[c(14,15), "ipc_log_predicho"] <- c(2.3708+(-0.0178), 2.3708+(-0.0178)+(0.0278))
e$ipc_exp <- exp(e$inf_log)
e$ipc_exp_predict <- exp(e$ipc_log_predicho)

e$error <- (e$ipc_exp - e$ipc_exp_predict)^2
mean(e$error, na.rm = T) %>% sqrt()

# grafico
x11()
 predict(t_var_1, n.ahead = 15, dumvar = as.matrix(rep(0,15))) %>% plot

# Hipótesis nula de autocorrelacion (rechazar)
seriala <- serial.test(x = t_var_1, 
                       lags.pt = 11, 
                       type = "PT.asymptotic")
seriala

# Hipótesis nula de normalidad (no rechazar)
normalidad <- normality.test(t_var_1)
normalidad

# Hipótesis nula de homoscedasticidad (no rechazar)
arch1 <- arch.test(t_var_1, lags.multi=11)
arch1

# Grafico normalidad de inflacion
res_ipc <- t_var_1$varresult$inf_log$residuals

ggplot() +
  geom_histogram(aes(x = res_ipc), fill = "steelblue1", col = "black") +
  theme_bw() +
  xlab("error") +
  ylab("cantidad")

# Curvas impulso respuesta
# irfs
irf_all <- irf(t_var_1, n.ahead = 12, boot = T)

par(mfrow = c(3,3))
x11()
plot(irf_all)

# Descomposición de la varianza
var1_fevd_d2ltp <- fevd(t_var_1, n.ahead=15)$inf_log
varianza <- data.frame(var1_fevd_d2ltp *100)
colores_azules <- c("#0072B2", "#009E73", "deeppink3")

varianza %>%
  mutate(id = seq(1:15)) %>%
  pivot_longer(-id, names_to = "Variable", values_to = "val") %>%
  ggplot() +
  aes(x = as.factor(id), group = Variable, col = Variable, y = val) +
  geom_line() +
  geom_point(size = 2) +
  xlab("Periodo") +
  scale_color_manual(values = colores_azules) +
  theme_bw() +
  theme(axis.title = element_text(size = 10),  # Ajustar tamaño de etiquetas de los ejes
        axis.text = element_text(size = 10),   # Ajustar tamaño de texto de los ejes
        legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda
        legend.position = "bottom",  # Posicionar leyenda abajo
        legend.box = "horizontal",   # Mostrar leyenda en formato horizontal
        legend.justification = "center")  # Centrar leyenda

x11()
plot(df$badlar_mensual)

# inflacion y estimacion
est_pred <- data.frame(
  id = seq(1:length(t_var_1$varresult$inf_log$model$y)),
  predicho = t_var_1$varresult$inf_log$fitted.values,
  real = t_var_1$datamat$inf_log
)

est_pred %>% 
  pivot_longer(-id, names_to = "serie", values_to = "inf_var" ) %>% 
  ggplot() + aes(x = id, y = inf_var, group = serie, col = serie) +
  geom_line()+
  theme_bw()

# Gráfico de comparación de modelos
pronosticos_agus <- readxl::read_excel("tp final/datos/predicciones.xlsx")
pronosticos_agus$prediccion_var <- e$ipc_exp_predict[1:15]
pronosticos_agus$Fecha <- as.Date(pronosticos_agus$Fecha)
colnames(pronosticos_agus) <- c("fecha", "Testing", "REM", "LSTM", "Prophet", "Naïve", "S. Naïve", "Var(8)")

# grafico
pronosticos_agus %>% 
  dplyr::select(-REM) %>% 
  pivot_longer(-fecha, names_to = "Modelo", values_to = "Pronostico") %>% 
  ggplot() +
  aes(x = fecha, y = Pronostico, col = Modelo) + 
  geom_line() +
  geom_point() +
  theme_light()

# Definir los colores para cada modelo
colores <- c("Testing" = "black", "LSTM" = "#e74c3c", "Prophet" = "#3498db", 
             "Naïve" = "#2ecc71", "S.Naïve" = "seagreen4", "Var(8)" = "#f39c12")

# Crear el gráfico
pronosticos_agus %>% 
  dplyr::select(-REM) %>% 
  pivot_longer(-fecha, names_to = "Modelo", values_to = "Pronostico") %>% 
  ggplot() +
  aes(x = fecha, y = Pronostico, col = Modelo) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = colores) +  
  theme_light()

# Define los límites de la sombra
fecha_inicio <- as.Date("2024-03-01")
fecha_fin <- as.Date("2024-06-01")

# Graficar con la paleta de colores personalizada y sombreado en el fondo
pronosticos_agus %>% 
  dplyr::select(-REM) %>% 
  pivot_longer(-fecha, names_to = "Modelo", values_to = "Pronostico") %>% 
  ggplot() +
  aes(x = fecha, y = Pronostico, col = Modelo) + 
  annotate("rect", xmin = fecha_inicio, xmax = fecha_fin, 
           ymin = -Inf, ymax = Inf, fill = "purple", alpha = 0.15) +
  theme(legend.position = "bottom") +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colores) +  
  theme_light() 
  