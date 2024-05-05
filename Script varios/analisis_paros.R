
# Usos de sube en paraná

# Librerias
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
d2024 <- read.csv("dat-ab-usos-2024.csv")
d2023 <- read.csv("dat-ab-usos-2023.csv")
d2022 <- read.csv("dat-ab-usos-2022.csv")
d2021 <- read.csv("dat-ab-usos-2021.csv")
data <- bind_rows(d2021, d2022, d2023, d2024)

# Filtro casos paraná y agrupo por dia
data <- data %>% 
  filter(MUNICIPIO == "PARANA") %>% 
  select(DIA_TRANSPORTE, LINEA, CANTIDAD) %>% 
  group_by(DIA_TRANSPORTE) %>% 
  summarise(cantidad = sum(CANTIDAD)) %>% 
  filter(DIA_TRANSPORTE > "2023-04-01")
data$DIA_TRANSPORTE <- as.Date(data$DIA_TRANSPORTE, format = "%Y-%m-%d")

# Defino si es fin de semana
dias_semana <- factor(weekdays(data$DIA_TRANSPORTE), levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
data$weekend <- ifelse(dias_semana %in% c("sábado", "domingo"), 1, 0)

# Definir los colores para cada día de la semana
colores_gossip_girl <- c("lunes" = "#93C2B4",     # Serena Van Der Woodsen
                         "martes" = "#FFA69E",    # Blair Waldorf
                         "miércoles" = "#1C1D21", # Chuck Bass
                         "jueves" = "#E0B1CB",    # Nate Archibald
                         "viernes" = "#E5D9F2",   # Dan Humphrey
                         "sábado" = "#FFD5C2",    # Jenny Humphrey
                         "domingo" = "#FEC8D8")   # Georgina Sparks

# Boxplot
data %>% 
  ggplot() +
  aes(y = cantidad, group = dias_semana, x = dias_semana) + 
  geom_boxplot(fill = "steelblue2") +
  scale_fill_manual(values = colores_gossip_girl) +  # Asignar los colores definidos
  theme_bw() +
  ylab("Pasajes") +
  xlab("") +
  labs(fill = "Día", title = "Cantidad de pasajes usados por día", subtitle = "En Paraná (2023-2024)")

# serie
data %>%
  ggplot() +
  aes(y = cantidad, x = DIA_TRANSPORTE) + 
  geom_line(color = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +  # Formato de fecha mensual
  theme_bw() +
  ylab("Pasajes") +
  xlab("") +
  labs(title = "Cantidad de pasajes usados por día",
       subtitle = "En Paraná (2023-2024)") +
  theme(axis.text.x = element_text(angle = 90))

# Analizamos la autocorrelación
g1 = ggAcf(diff(data$cantidad), lag.max = 100) + ggtitle("Autocorrelación total") + theme_light()  # MA 
g2 = ggPacf(diff(data$cantidad), lag.max = 100) + ggtitle("Autocorrelación parcial") + theme_light() # AR
grid.arrange(g1,g2, nrow = 1)

# agrego paros
paros <- c("2024-02-26", "2024-02-13", "2024-03-13", "2024-03-14", "2024-03-05")
paros <- if_else(data$DIA_TRANSPORTE %in% as.Date(paros), 1, 0)
boxplot(data$cantidad ~ paros)

data$paros <- paros

# Externalizar
auto.arima(data$cantidad, xreg = as.matrix(data$weekend, data$paros))
arima_1 <- arima(data$cantidad, order = c(1,1,2), seasonal = c(7,0,0), xreg = as.matrix(data$weekend, data$paros))
summary(arima_1)
coeftest(arima_1)

# regresion
boxplot(data$cantidad ~ data$weekend, color = "orange")
t.test(data$cantidad ~ data$weekend)

# Promedio mensual
data %>% 
  group_by(month(DIA_TRANSPORTE)) %>% 
  summarise(mean = mean(cantidad))

data$es_abril <- if_else(month(data$DIA_TRANSPORTE) >= 4 & month(data$DIA_TRANSPORTE) < 12, 1, 0)
t.test(data$cantidad, data$es_abril)

data %>% 
  group_by(as.factor(es_abril)) %>% 
  summarise(mean = mean(cantidad))

# Promedio diario
data$tipo_dia <- case_when(
  dias_semana %in% c("sábado") ~ "sabado",
  dias_semana == "domingo" ~ "domingo", 
  .default = "semana"
)

data %>% 
  group_by(as.factor(paros)) %>% 
  summarise(mean = mean(cantidad))

