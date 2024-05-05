suppressPackageStartupMessages({
  library(forecast)
  library(ggplot2)
  library(dplyr)
  library(astsa)
  library(tseries)
  library(xts)
  library(tidyverse)
  library(magrittr)
  library(ggfortify)
  library(forecast)
  library(dygraphs)})


library("readxl")
datos <- read_excel("BD Tp3.xlsx")
datos <- datos %>% select(Fecha, Var_IPC)
df <- data.frame(datos$Var_IPC)
rownames(df) <-datos$Fecha 
rownames(datos) <- datos$Fecha
tsipc <- ts(df,  frequency=12,  start=c(2011,01))
training <- window(tsipc, start = 2011,   end = c(2023,3))
testing <- window(tsipc, start = c(2023,4))

mod_naive <- naive(training, 12)
mod_snaive <- snaive(training, 12)
mod_drift <- rwf(training,drift = TRUE,h = 12)

checkresiduals(mod_naive)
checkresiduals(mod_snaive)
checkresiduals(mod_drift)

accuracy(mod_naive, testing)
accuracy(mod_snaive, testing)
accuracy(mod_drift, testing)

x11()
plot(training, main = "IPC", ylab = "%", xlab = "Meses")
lines(mod_naive$mean, col = 4)
lines(mod_snaive$mean, col = 5)
lines(mod_drift$mean, col = 3)
legend("topleft", 
       lty=1, 
       col=c(4,2),
       legend = c("Naive","SNaive","drift"),
       bty = "n")

lines(testing, col="red")