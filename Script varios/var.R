
# Var
df <- readxl::read_excel("tp final/datos/bdfinal.xlsx")
df$ipc_log_ipc <- log(df$ipc_var)

ipc_estacional <- decompose(ts(df$ipc_var, start = c(2011,01), frequency = 12), type = "additive")
ipc_estacional <- ipc_estacional$seasonal

df$ipc_var <- df$ipc_var - ipc_estacional

# Granger
df$fecha <- as.Date(df$fecha)

# Formato
series <- df %>% 
  dplyr::select(-fecha) %>% 
  ts(start = c(2011,01), frequency = 12)

series[,"ccl_var"] <- na.approx(series[,"ccl_var"]) %>% matrix()

# Estacionariedad
library(tseries)

# Primeras diferencias
series_dif <- diff(series)

for (i in 1:ncol(series_dif)) {
  result <- adf.test(series_dif[,i])[[4]]
  print("##########")
  print(paste("VARIABLE", colnames(series_dif)[i]))
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
      granger_result <- grangertest(series_dif[,i] ~ series_dif[,j])[2,4]
      
      # Almacenar el p-valor en la matriz
      matriz_granger[i, j] <- granger_result
    }else{
      matriz_granger[i,j] <- 0
    }
  }
}

melted_matriz_granger <- melt(matriz_granger)

m_granger1 <- ggplot(data = melted_matriz_granger, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "#4c7b8f") +
  labs(title = "P-valores de la causalidad de Granger",
       x = "Variable Y",
       y = "Variable X") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")
x11()
m_granger1

# Pairs
x11()
GGally::ggpairs(as.data.frame(var_df),
                upper = list(continuous = GGally::wrap("smooth", method = "lm", se = FALSE, col = "steelblue")), 
                title = "Bivariados simples")


# exogenas
semestre_2023 <- if_else(year(df$fecha[-1]) == 2023 & month(df$fecha[-1]) >= 6, 1, 0)
outlier_1416 <- if_else(year(df$fecha[-1]) %in% c(2014,2016), 1, 0)
outlier_2023 <- if_else(df$fecha[-1] %in% c(as.Date("2023-12-01"), as.Date("2024-01-01")), 1, 0)

# Seleccion de var

# Seleccion de variables y dummies
variables <- c("ipc_log_ipc", "tcm_var", "iR")
var_df <- series_dif[, colnames(series_dif) %in% variables]

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

# Train y test
training <- window(series_dif,
                   start = c(2011,02), 
                   end = c(2023,03))

testing <- window(series_dif, 
                  start = c(2023,04), 
                  end = c(2024,03))

# Modelo y pronostico

# exogena 2014 y 2016
out_1416 <- if_else(year(df$fecha[-1]) %in% c(2014,2016) & month(df$fecha[-1]) %in% c(1,2), 1, 0)
colnames(out_1416) <- "exo1"

variables <- c("ipc_log_ipc", "a3500_var", "badlar_mensual")
var_df <- training[, colnames(training) %in% variables]
vars::VARselect(var_df, exogen = out_1416[1:146], type = "both")

# modelo
t_var_1 <- VAR(var_df, p = 8)
t_var_1 %>% summary()

# forecast y validación cruzada
var_forecast <- predict(t_var_1, n.ahead = 15, dumvar = as.matrix(rep(0,15)))
ipc_forecast <- var_forecast$fcst$ipc_log_ipc

# RMSE
e <- data.frame(predicho = ipc_forecast[,1],
                real = testing[,"ipc_log_ipc"] %>% c())

e$dif <- (e$real - e$predicho)^2
e$fecha <- seq(as.Date("2023-04-01"), as.Date("2024-03-01"), by = "month")

x11()
predict(t_var_1, n.ahead = 15, dumvar = as.matrix(rep(0,15))) %>% plot

# sumo las diferencias
df_forecast <- df %>% 
  filter(fecha > as.Date("2023-02-01")) %>% 
  dplyr::select(fecha, ipc_log_ipc) %>% 
  mutate(predicho = c(NA, e$predicho),
         real = c(NA, e$real))

df_forecast$ipc_log_predicho <- lag(df_forecast$ipc_log_ipc) + df_forecast$predicho
df_forecast$ipc_exp <- exp(df_forecast$ipc_log_ipc)
df_forecast$ipc_exp_predict <- exp(df_forecast$ipc_log_predicho)

df_forecast$e_ipc <- (df_forecast$ipc_exp - df_forecast$ipc_exp_predict)^2

mean(df_forecast$e_ipc, na.rm = T) %>% sqrt()

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

# Curvas impulso respuesta
# irfs
irf_all <- irf(t_var_1, impulse = "ipc_log_ipc", n.ahead = 12, boot = T)

par(mfrow = c(3,3))
x11()
plot(irf_all)

# Descomposición de la varianza
var1_fevd_d2ltp <- fevd(t_var_1, n.ahead=12)$ipc_log_ipc
var1_fevd_d2ltp 

x11()
plot(df$badlar_mensual)

# Gráfico de comparación de modelos
df %>% filter(fecha > "2022-01-01") %>% 
  ggplot() +
  geom_line(aes(x = fecha, y = ipc_var)) + 
  geom_line(data = df_forecast, aes(x = fecha, y = ipc_exp_predict), col = "blue") + 
  theme_bw()
