serieDatos <- tibble(
  Fecha = seq.Date(as.Date("1949-01-01"), as.Date("1960-12-01"), by = "1 month"),
  y = as.numeric(data),
  type = rep(c("Entrenamiento","Test"),c(132,12))
)

get.window <- function(serie=serie,inicio,fin){
  require(lubridate)
  from <- c(year(inicio),month(inicio))
  to <- c(year(fin),month(fin))
  return(window(serie,from,to))
}

get.prediction <- function(train,H){
  pred_naive <- rwf(train, h = H) %> % as_tibble() %> % pull(`Point Forecast`) %> % last()
  pred_snaive <- snaive(train,h = H) %> % as_tibble() %> % pull(`Point Forecast`) %> % last()
  return(list(naive=pred_naive,seasonal=pred_snaive))
}

ggplot(serieDatos, aes(x = Fecha, y = y)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "black") +
  scale_x_date("Fecha", date_breaks = "2 year" ,date_labels = " %Y",
               limits = as.Date(c("1949-01-01","1961-01-01"))) +
  ylab("Pasajeros (miles)") +
  theme(panel.grid.minor.x = element_blank())

train <- window(data,start=1949,end=c(1959,12))
test <- window(data,start=1960,end=c(1960,12))
trainSeq <- seq.Date(as.Date("1949-01-01"), as.Date("1959-12-01"), by = "1 month")
testSeq <- seq.Date(as.Date("1960-01-01"), as.Date("1960-12-01"), by = "1 month")

modelo_naive <- rwf(train, h = 12)
modelo_snaive <- snaive(train,h = 12)

modelos_simples <- bind_rows(
  as_tibble(modelo_naive) %>% cbind(Fecha = testSeq) %>%
    cbind(Type = "Naïve"),
  as_tibble(modelo_snaive) %>% cbind(Fecha = testSeq) %>%
    cbind(Type = "Naïve Estacional"),
) %>% as_tibble %>% bind_cols(Real = rep(as.numeric(test), 2)) %>%
  mutate(Residual = Real - `Point Forecast`)

textsize <- 10


# plot modelos simples
modelos <- ggplot(serieDatos, aes(x = Fecha, y = y, col = type)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_point(
    data = modelos_simples,
    aes(x = Fecha, y = `Point Forecast`, col = Type),
    inherit.aes = F,
    size = 0.5) +
  geom_line(
    data = modelos_simples,
    aes(x = Fecha, y = `Point Forecast`, col = Type),
    inherit.aes = F) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "black") +
  scale_x_date(
    "Fecha",
    date_breaks = "2 year" ,
    date_labels = " %Y",
    limits = as.Date(c("1949-01-01", "1961-01-01"))
  ) +
  scale_color_manual(breaks=c("Entrenamiento","Test","Naïve","Naïve Estacional"),
                     values=c("#45caff","#ff1b6b","#ff9f1c","#00a7e1")) +
  ylab("Pasajeros (miles)") +
  labs(col = "", fill = "") +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = c(.14,.75),
        legend.text = element_text(size=8),
        legend.background = element_blank(),
        legend.key = element_blank(),
        text = element_text(size=textsize))

# residuos
r1 <- modelos %>% filter(Type=="Nauve") %>%
  ggplot(aes(x = Fecha, xend= Fecha, y = 0 , yend = Residual)) +
  geom_segment() + scale_x_date(
    "Fecha",
    date_breaks = "1 month" ,
    date_labels = " %m/ %y",
    limits = as.Date(c("1960-01-01", "1961-01-01"))
  ) + ylim(-20,250) + ylab("e") + theme(text = element_text(size=textsize),
                                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                        panel.grid.minor.x = element_blank())

r2 <- modelos_simples %>% filter(Type=="Naïve Estacional") %>%
  ggplot(aes(x = Fecha, xend= Fecha, y = 0 , yend = Residual)) +
  geom_segment() + scale_x_date(
    "Fecha",
    date_breaks = "1 month" ,
    date_labels = " %m/ %y",
    limits = as.Date(c("1960-01-01", "1961-01-01"))
  ) + ylim(-20,250) + ylab("e") + theme(text = element_text(size=textsize),
                                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                        panel.grid.minor.x = element_blank())

acfr1 <- modelos_simples %> %
  filter(Type=="Naïve") %> %
  pull(Residual) %> %
  ggAcf(plot=T,lag.max = 12) +
  ylim(-0.8,0.8) +
  ylab("FAC") +
  theme(plot.title=element_blank()) +
  theme(text = element_text(size=textsize),
        panel.grid.minor.x = element_blank())

acfr2 <- modelos_simples %> %
  filter(Type=="Naïve Estacional") %> %
  pull(Residual) %> %
  ggAcf(plot=T,lag.max = 12) +
  ylim(-0.8,0.8) +
  ylab("FAC") +
  theme(plot.title=element_blank()) +
  theme(text = element_text(size=textsize),
        panel.grid.minor.x = element_blank())

modelos / (r1 + r2) / (acfr1 + acfr2) + plot_annotation(tag_levels = "a")
modelos_simples %> %
  select(-c(2,3,4,5)) %> %
  mutate(First = month(min(Fecha)),
         H = month(Fecha) - First + 1) %> %
  select(Type,H,Residual) %> %
  mutate(Residual = Residual) %> %
  pivot_wider(id_cols="Type",names_from="H",values_from="Residual") %> %
  kable(col.names = c("Modelo",paste0("$\\ell=",1:12,"$")),
        escape = F,align = "llcccc",booktabs=T, format="latex",
        caption = "Errores de estimación en los distintos horizontes de predicción,
para los dos modelos simples propuestos\\label{tabla1}") %> %
  kable_styling(full_width = F,latex_options = c("hold_position"),font_size = 8)
modelos_simples %> %
  select(-c(2,3,4,5)) %> %
  mutate(First = month(min(Fecha)),
         H = month(Fecha) - First + 1) %> %
  select(Type,H,Residual) %> %
  mutate(Residual = abs(Residual)) %> %
  group_by(Type) %> %
  mutate(MSE = cumsum(Residual^2)) %> %
  ungroup() %> %
  mutate(MSE = MSE/H) %> %
  select(-Residual) %> %
  mutate(MSE = round(MSE,0)) %> %
  pivot_wider(id_cols="Type",names_from="H",values_from="MSE") %> %
  kable(col.names = c("Modelo",paste0("$H=",1:12,"$")),
        escape = F,align = "llcccc",booktabs=T, format="latex",
        caption = "MSE para distintos horizontes de predicción $H$,
para cada uno de los modelos simples propuestos\\label{tabla2}") %> %
  kable_styling(full_width = F,latex_options = c("hold_position"),font_size = 8)
cv <- tibble(Till = zoo::as.Date(time(serie)), From = min(Till)) %> %
  select(From,Till) %> %
  mutate(Months = as.numeric(round((Till - From)/(365.25/12)))) %> %
  crossing(H = 1:12) %> %
  mutate(train = map2(From,Till,~get.window(serie,.x,.y))) %> %
  mutate(testDate = Till %m+ % months(H)) %> %
  left_join(serieDatos, by = c("testDate" = "Fecha")) %> %
  select(-type) %> %
  filter(Months>=24) %> %
  mutate(f = map2(train,H,get.prediction)) %> %
  unnest_wider(f) %> %
  select(-train) %> %
  pivot_longer(cols = c(naive,seasonal),names_to = "Type",values_to = "Prediction") %> %
  mutate(Residual = y - Prediction) %> %
  arrange(Type,Months,H) %> %
  group_by(Type,Months) %> %
  mutate(MSE = cumsum(Residual^2)) %> %
  ungroup() %> %
  mutate(MSE = MSE/H)
ggplot(cv,aes(x=as.factor(H),y=MSE,fill=Type)) +
  geom_boxplot(outlier.size = 0.5) +
  xlab("H") +
  scale_y_continuous(breaks = seq(0,30000,5000)) +
  scale_fill_manual(values = c("#ff9f1c","#00a7e1"),
                    labels = c("Naïve","Naïve\nEstacional")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank())
ggplot(serieDatos, aes(x = Fecha, y = log(y))) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "black") +
  scale_x_date("Fecha", date_breaks = "2 year" ,date_labels = " %Y",
               limits = as.Date(c("1949-01-01","1961-01-01"))) +
  ylab("log(Pasajeros)") +
  theme(panel.grid.minor.x = element_blank())
serieDatos %> %
  filter(type=="Entrenamiento") %> %
  pull(y) %> %
  log() %> %
  ggAcf(plot=T) +
  ylab("FAC") +
  theme(plot.title=element_blank()) +
  theme(text = element_text(size=textsize),
        panel.grid.minor.x = element_blank())
p1 <- serieDatos %> %
  filter(type=="Entrenamiento") %> %
  pull(y) %> %
  log() %> %
  diff() %> %
  diff(lag=12) %> %
  ggAcf(plot=T) +
  ylab("FAC") +
  theme(plot.title=element_blank()) +
  theme(text = element_text(size=textsize),
        panel.grid.minor.x = element_blank())
p2 <- serieDatos %> %
  filter(type=="Entrenamiento") %> %
  pull(y) %> %
  log() %> %
  diff() %> %
  diff(lag=12) %> %
  ggAcf(plot=T, type = "partial") +
  ylab("FACP") +
  theme(plot.title=element_blank()) +
  theme(text = element_text(size=textsize),
        panel.grid.minor.x = element_blank())
p1 / p2 + plot_annotation(tag_levels = "a")
x <- train %> % log %> % diff(lag = 12) %> % diff
matrixAIC <- AIC_Matrix(x,
                        p.order = 4,
                        q.order = 4)
dv <- as_tibble(matrixAIC) %> %
  cbind("AR"=0:4) %> %
  pivot_longer(cols=-AR,names_to="MA",values_to="AIC")
p1<-ggplot(dv,aes(x=MA,y=AR,fill=AIC,label=round(AIC,0))) +
  geom_tile() +
  geom_text(color="white") +
  scale_fill_distiller(palette = "Reds") +
  scale_x_discrete(position="top",expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),trans = "reverse") +
  theme(plot.margin=unit(c(0,0,0,0),"mm"))
matrixBIC <- BIC_Matrix(x,
                        p.order = 4,
                        q.order = 4)
dv <- as_tibble(matrixBIC) %> %
  cbind("AR"=0:4) %> %
  pivot_longer(cols=-AR,names_to="MA",values_to="BIC")
p2<-ggplot(dv,aes(x=MA,y=AR,fill=BIC,label=round(BIC,0))) +
  geom_tile() +
  geom_text(color="white") +
  scale_fill_distiller(palette = "Blues") +
  scale_x_discrete(position="top",expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),trans = "reverse") +
  # coord_fixed() +
  theme(plot.margin=unit(c(0,0,0,0),"mm"))
p1 + p2
modelo <- arima(x,c(2,0,3),include.mean = F)
modelo$residuals %> %
  ggAcf(plot=T) +
  ylab("FAC") +
  theme(plot.title=element_blank()) +
  theme(text = element_text(size=textsize),
        panel.grid.minor.x = element_blank())
pred <- predict(modelo,12)$pred
x.completada<-c(x,pred)
xinv1<-diffinv(x.completada,lag=1,xi=(train %> % log %> % diff(lag = 12))[1])
xinv2<-diffinv(xinv1,lag=12,xi=(train %> % log)[1:12])
serieDatos %> % cbind(Pred = exp(xinv2)) %> %
  filter(type=="Test") %> %
  select(Fecha,Pred,type) %> %
  mutate(type = "ARIMA") %> %
  rename(y = Pred) %> %
  bind_rows(serieDatos) %> %
  ggplot(aes(x = Fecha, y = y, col = type)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "black") +
  scale_x_date(
    "Fecha",
    date_breaks = "2 year" ,
    date_labels = " %Y",
    limits = as.Date(c("1949-01-01", "1961-01-01"))
  ) +
  scale_color_manual(breaks=c("Entrenamiento","Test","ARIMA"),
                     values=c("#45caff","#ff1b6b","#7400b8")) +
  ylab("Pasajeros (miles)") +
  labs(col = "", fill = "") +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = c(.14,.85),
        legend.text = element_text(size=8),
        legend.background = element_blank(),
        legend.key = element_blank(),
        text = element_text(size=textsize))
modelos_simples %> %
  select(-c(2,3,4,5)) %> %
  mutate(First = month(min(Fecha)),
         H = month(Fecha) - First + 1) %> %
  select(Type,H,Residual) %> %
  bind_rows(serieDatos %> % cbind(Pred = exp(xinv2)) %> %
              filter(type=="Test") %> %
              rename(Type = type) %> %
              mutate(Type = "ARIMA",
                     H = 1:12,
                     Residual = y - Pred) %> %
              select(Type,H,Residual)) %> %
  group_by(Type) %> %
  mutate(MSE = cumsum(Residual^2)) %> %
  ungroup() %> %
  mutate(MSE = MSE/H) %> %
  select(-Residual) %> %
  mutate(MSE = round(MSE,0)) %> %
  pivot_wider(id_cols="Type",names_from="H",values_from="MSE") %> %
  kable(col.names = c("Modelo",paste0("$H=",1:12,"$")),
        escape = F,align = "llcccc",booktabs=T, format="latex",
        caption = "MSE para distintos horizontes de predicción $H$,
para el modelo ARIMA\\label{tabla3}") %> %
  kable_styling(full_width = F,latex_options = c("hold_position"),font_size = 8)
labs = knitr::all_labels()
labs = setdiff(labs, c("setup", "paquetes"))
