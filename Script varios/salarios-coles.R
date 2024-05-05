library(tidyverse)

df <- read.csv("w_median_privado_mensual_por_clae6.csv")
df_mean <- read.csv("w_mean_privado_mensual_por_clae6.csv")
dict <- read.csv("clae_agg.csv")
dict <- dict %>% select(clae6, clae6_desc)

df_total <- left_join(df_mean, dict, by = "clae6")
df_total$fecha <- as.Date(df_total$fecha)

# Servicio de transporte automotor urbano y suburbano regular de pasajeros 
df_total %>% 
  filter(fecha == as.Date("2023-03-01")) %>% 
  filter(gsub("Servicio de transporte automotor urbano y suburbano regular", clae6_desc))

# Servicio de transporte automotor
evolucion_salario <- df_total %>% 
  filter(clae6 == 492110)

# Ggplot
evolucion_salario %>%
  mutate(diciembre = case_when(month(fecha) == 6 ~ "paritaria", month(fecha) == 12 ~ "paritaria")) %>% 
  ggplot() +
  aes(x = fecha, y = w_mean) +
  geom_line() +
  geom_point(aes(col = diciembre)) +
  theme_light() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Mostrar cortes de año en el eje x
  labs(x = "Año", y = "Diferencia") +
  theme(axis.text.x = element_text(angle = 90))

# diferenciacion para ver incrementos
evolucion_salario$dif <- c(NA, diff(evolucion_salario$w_mean))

# Ggplot
evolucion_salario %>%
  mutate(diciembre = case_when(month(fecha) == 6 ~ "paritaria", month(fecha) == 12 ~ "paritaria")) %>% 
  ggplot() +
  aes(x = fecha, y = dif) +
  geom_line() +
  geom_point(aes(col = diciembre)) +
  theme_light()  +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Mostrar cortes de año en el eje x
  labs(x = "Año", y = "Diferencia") +
  theme(axis.text.x = element_text(angle = 90))

