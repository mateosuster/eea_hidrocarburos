#limpio env y garbage collector
rm(list=ls())
gc()

library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tsibble)
library(feasts)

# para gaussian process
library(kernlab)


setwd("~/dataScience/maestriaDC/2021/EEA_2021cuat2/practica/tp_final/eea_hidrocarburos")

# arch <- "dataset_2013a2020.csv"
arch <- "produccion_2013a2020_corregido.csv"


# función de agregacion
agregacion <- function(x) {
  x %>% 
    summarise(prod_pet = sum(prod_pet, na.rm = T),
              prod_gas = sum(prod_gas, na.rm = T),
              prod_agua = sum(prod_agua, na.rm = T),
              tef_avg = mean(tef_avg, na.rm = T),
              vida_util_avg = mean(vida_util_avg, na.rm = T)#,
              # inversion_explotacion = sum(inversion_explotacion, na.rm = T),
              # inversion_exploracion = sum(inversion_exploracion, na.rm = T),
              # precio_interno_crudo = mean(precio_interno_crudo, na.rm = T),
              # precio_interno_gas = mean(precio_interno_gas, na.rm = T),
              # riesgo_pais = mean(riesgo_pais, na.rm = T)
    )
}



#levanto datos de producción
data <- read_csv(paste0("data/resultados/",arch), 
                 locale = locale(encoding = "ISO-8859-1")) %>%
  filter(anio %in% c(2013:2020)) %>%                     # filtro años para mayor velocidad
  mutate(fecha =  yearmonth(paste(anio, mes, sep="/")) ,
         n_dias = days_in_month(fecha),
         prod_pet = prod_pet/n_dias,
         prod_gas = prod_gas/n_dias,
         prod_agua = prod_agua/n_dias,
         anio = year(fecha),
         mes = month(fecha)) %>%
  dplyr::select(-n_dias) %>%
  group_by(fecha,anio,mes) %>% agregacion() #%>% dplyr::select(c(anio,mes,prod_pet,fecha))


data <- data %>% ungroup() %>% mutate(ind = row_number())

# data <- data %>% dplyr::select(c(anio,mes,ind,prod_pet,fecha))



########### GP

#datos reales: 96 es 2013 a 2020 inc.
real_hasta <- 96
x <- seq(1,real_hasta,1)
x_label <- data[data$ind<=real_hasta,]$fecha
y <- data[data$ind<=real_hasta,]$prod_pet


# entreno GP con datos train: 84 meses es 2013 a 2019 inc.
train_hasta <- 84
x_train <- seq(1,train_hasta,1)
y_train <- scale(y[1:train_hasta])
gp <- gausspr(x_train, y_train[,1], type = "regression", kernel = "rbfdot", 
              variance.model = TRUE, scaled = TRUE, cross = 5)

# escaleo con parámetros de train
y <- scale(y, center=attr(y_train, 'scaled:center'), scale = attr(y_train, 'scaled:scale'))[,1]
plot(x,y, ylim=c(-4,4), axes=FALSE, frame.plot=TRUE, ylab="Prod. Crudo (ref. al promedio)")
axis(side=1, labels=x_label, at=x)
axis(side=2,at=seq(-10,10,1))



# Posterior predict en datos test: 2013 a 2020 inc.
test_hasta <- length(x)
x_test <- seq(1,test_hasta,0.5)
y_test <- predict(gp, x_test)
lines(x_test, y_test, col="blue", type = "p")

# línea vertical hasta donde entreno
abline(v=train_hasta, col="red")

# banda a 1 desvío
lines(x_test, predict(gp, x_test)+1*predict(gp,x_test, type="sdeviation"),col="blue")
lines(x_test, predict(gp, x_test)-1*predict(gp,x_test, type="sdeviation"),col="blue")

# banda a dos devíos
lines(x_test, predict(gp, x_test)+2*predict(gp,x_test, type="sdeviation"),col="black")
lines(x_test, predict(gp, x_test)-2*predict(gp,x_test, type="sdeviation"),col="black")

grid()








