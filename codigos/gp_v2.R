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

arch <- "dataset_2013a2020.csv"


# función de agregacion
agregacion <- function(x) {
  x %>% 
    summarise(prod_pet = sum(prod_pet, na.rm = T),
              prod_gas = sum(prod_gas, na.rm = T),
              prod_agua = sum(prod_agua, na.rm = T),
              tef_avg = mean(tef_avg, na.rm = T),
              vida_util_avg = mean(vida_util_avg, na.rm = T),
              inversion_explotacion = sum(inversion_explotacion, na.rm = T),
              inversion_exploracion = sum(inversion_exploracion, na.rm = T),
              precio_interno_crudo = mean(precio_interno_crudo, na.rm = T),
              precio_interno_gas = mean(precio_interno_gas, na.rm = T),
              riesgo_pais = mean(riesgo_pais, na.rm = T)
    )
}



#datos originales
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

data <- data %>% dplyr::select(c(anio,mes,ind,prod_pet))






########### GP

#predict and variance
real_hasta <- 72
x <- seq(1,real_hasta,1)
y <- data[data$ind<=real_hasta,]$prod_pet
plot(x,y)#, ylim=c(-4*10^5,12*10^5))

train_hasta <- 66
xgp <- seq(1,train_hasta,1)
ygp <- data[data$ind<=train_hasta,]$prod_pet
# ygp <- scale(ygp)[,1]
gp <- gausspr(xgp, ygp, variance.model = TRUE)

test_hasta <- 72
xtest <- seq(1,test_hasta,0.05)
lines(xtest, predict(gp, xtest))

abline(v=train_hasta, col="red")

lines(xtest,
      predict(gp, xtest)+2*predict(gp,xtest, type="sdeviation"),
      col="red")
lines(xtest,
      predict(modelo2, xtest)-2*predict(gp,xtest, type="sdeviation"),
      col="red")







