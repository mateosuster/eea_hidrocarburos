rm(list=ls())
gc()

library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(modeest)


### CAMBIAR según PC
setwd("~/dataScience/maestriaDC/2021/EEA_2021cuat2/practica/tp_final/eea_hidrocarburos")
setwd("C:/Archivos/repos/eea_hidrocarburos")


######################
# Parámetros dataset producción
#####################
anio_desde = 2013
anio_hasta = 2020

# chequar la coherencia del agrupamiento de produccion e inversión (los precios SOLO estan por cuenca)
group_cols_prod = c( "anio", "mes", 
                "cuenca", 
                "areayacimiento" , "areapermisoconcesion",
                "clasificacion",
                "tipo_de_recurso" ,"tipopozo", "tipoextraccion",
                "proyecto")

group_cols_inv = c("anio", "cuenca",
                   "area_per_conc", "concepto"
                   )

name_dataset_export = "dataset_2013a2020.csv" # ir cambiando segun lo que se quiera probar
name_dataset_produccion = "produccion_2013a2020_corregido.csv" # 

#NOTA: ajustar el join a manopla

#UNIDADES en las que terminan las vars
# PRODUCCION: crudo m3, gas Miles de m3, agua m3
# PRECIOS: crudo m3/USD, gas Miles de m3/USD (TC BNA comprador)
# INVERSIONES: Millones de USD

#################
#PRODUCCION 
################
# lista de archivos 
file_list = list.files(path ="data/prod_capiv/" , pattern="*.csv")
data_list <- list()

#guardo archivos en lista
for (i in seq_along(file_list)) {
  filename = file_list[[i]]
  anio = as.double(str_sub(filename , -8, -5 ))
  
  # Guardo DF en la lista
  if (anio %in% seq(anio_desde, anio_hasta)){
    # Read data in
    df <- read_csv(paste0("data/prod_capiv/", filename),
                   locale = locale())
    
    name = paste0("data_", anio )
    data_list[[name]] <- df
    cat("ya cargó el año", anio, "\n")
    
    rm(df)
    gc() 
  }
  if(i == length(file_list)) cat( "Listo !")
}


# asigno archivos
for (i in names(data_list)){
  # i <<- data.frame(data_list[[i]])
  assign(x = i, 
         value = data.frame(data_list[[i]]) %>%
           filter(clasificacion == "EXPLOTACION" ) %>% 
           mutate(cuenca = case_when(cuenca %in% c( "TOTAL CUENCA" ,"CAÑADON ASFALTO", 
                                                    "ÑIRIHUAU" , NA  ) ~ "OTRA",
                                     T ~cuenca)) %>% 
           mutate(idareapermisoconcesion = as.double(idareapermisoconcesion)) %>%
           group_by_at(group_cols_prod ) %>% 
           summarise(prod_pet = sum(prod_pet, na.rm = T), 
                     prod_gas = sum(prod_gas, na.rm = T),
                     prod_agua = sum(prod_agua, na.rm = T),
                     tef_avg = mean(tef, na.rm = T),
                     vida_util_avg = mean(vida_util, na.rm = T),
                     # tipoextraccion_mod = mlv(tipoextraccion, method = "mfv")         
                     # ,proyecto  = mlv(proyecto , method = "mfv")  ,       
                     # tipo_de_recurso  = mlv(tipo_de_recurso , method = "mfv")         
                     ) 

         )
}

dfs = sapply(.GlobalEnv, is.data.frame) 
super_data = do.call(rbind, mget(names(dfs)[dfs]))
rm(list=ls(pattern="^data_"))
gc()
glimpse(super_data)

# PRODUCCION  2018 (sirve de prueba)
# prod  = read_csv("data/prod_capiv/produccin-de-pozos-de-gas-y-petrleo-2018.csv")
# glimpse(prod)
# unique(prod$cuenca)
# unique(prod$formacion)
# table(prod$proyecto     )
# table(prod$clasificacion )
# table(prod$tipo_de_recurso )
# table(prod$sub_tipo_recurso )
# table(prod$tipoextraccion )
# table(prod$tipoestado)
# table(prod$tipopozo)

 
# #producción por area de pozos en explotación
# prod_area = prod %>%
#   filter(clasificacion == "EXPLOTACION" ) %>%
#   mutate(cuenca = case_when(cuenca %in% c( "TOTAL CUENCA" ,"CAÑADON ASFALTO",
#                                            "ÑIRIHUAU" , NA  ) ~ "OTRA",
#                             T ~cuenca)) %>%
#   mutate(idareapermisoconcesion = as.double(idareapermisoconcesion)) %>%
#   group_by_at( group_cols_prod  ) %>% #, empresa, idempresa
#   summarise(prod_pet = sum(prod_pet, na.rm = T),
#             prod_gas = sum(prod_gas, na.rm = T),
#             prod_agua = sum(prod_agua, na.rm = T))
# glimpse(prod_area)


#POZOS
# pozos = read_csv("data/listado-de-pozos-cargados-por-empresas-operadoras.csv")
# glimpse(pozos)
# table(pozos$formprod )

###################
# INVERSIONES
##################
inversiones = read_csv("data/resolucin-2057-inversiones-realizadas-ao-anterior.csv", 
                     col_types = cols(`Fecha Fin Tareas` = col_date(format = "%d/%m/%Y"), 
                                      `Fecha Inicio Tareas` = col_date(format = "%d/%m/%Y"))) %>% 
  rename(anio = "Año de presentación de la DDJJ",
       empresa = "Empresa informante",
       area_per_conc = "Área/Permiso/Concesión",
       yacimiento = "Yacimiento", cuenca = "Cuenca",
       concepto = "Descripción del plan de acción (Conceptos)",
       cantidad_exploracion = "Cant. Exploracion",
       cantidad_explotacion = "Cant. Explotacion",
       cantidad_exploracion_complementaria = "Cant. Exploracion Complementaria",
       exploracion_millones_usd = "Millones u$s Exploracion",
       explotacion_millones_usd = "Millones u$s Explotacion",
       exploracion_compl_millones_usd = "Millones u$s Exp. Complementaria") %>% 
  # select(anio, empresa, idempresa, concepto,
  #        area_per_conc, yacimiento, cuenca, 
  #        cantidad_exploracion, cantidad_explotacion,cantidad_exploracion_complementaria,
         # exploracion_millones_usd,explotacion_millones_usd, exploracion_compl_millones_usd )%>%
  # group_by(cuenca, area_per_conc) %>%
  mutate(anio = anio-1) %>% 
  group_by_at(group_cols_inv) %>% #empresa, idempresa
  summarise(inversion_explotacion = sum(explotacion_millones_usd, na.rm = T),
            inversion_exploracion = sum(exploracion_millones_usd, na.rm = T))

#exploracion de inversiones
glimpse(inversiones)
unique(inversiones$concepto)
inv_tot = inversiones %>% 
  # group_by(anio, concepto) %>%
  group_by(anio) %>%
  summarise(inv_tot = sum(inversion_explotacion)) %>% 
  arrange(-inv_tot)

inv_cuenca = inversiones %>% 
  group_by(anio, concepto) %>%
  summarise(inv_tot = sum(inversion_explotacion)) %>% 
  arrange(-inv_tot)

write.csv(inv_tot, file = "data/resultados/inversion_total.csv")
write.csv(inv_cuenca, file = "data/resultados/inversion_cuenca.csv")

###################
# tc bna
##################
tc = read.csv("data/tc_bna.csv") %>% 
  group_by(anio, mes) %>% 
  summarise(tc = mean(compra, na.rm = T))

###############
# ipc_us
###############

##########################
# precios de gas 
#######################
precio_gas = read_delim("data/precio_mercado_interno_gas_regalias.csv",   
                        delim = ";") %>% 
  rename(anio = "AÑO", mes = "MES") %>% 
  mutate(unidad_gas = "ARS/Mm3",         
         anio =zoo::na.locf(anio),
         fecha = as.Date(parse_date_time(paste0(anio, mes), orders = "ym"))) %>% 
  select( mes, anio, unidad_gas, everything(.)) %>% 
  pivot_longer(., cols = c("CUENCA AUSTRAL OFF-SHORE" , "CUENCA AUSTRAL ON-SHORE",  
                           "CUENCA CUYANA",  "CUENCA GOLFO SAN JORGE"  ,
                           "CUENCA NEUQUINA", "CUENCA NOROESTE", "TOTAL CUENCA"),
               names_to = "cuenca", values_to = "precio_interno_gas") %>% 
  mutate(
         precio_interno_gas = as.double(str_replace(precio_interno_gas, ",", "")),
         cuenca = case_when( 
                        cuenca == "CUENCA AUSTRAL OFF-SHORE" ~ "AUSTRAL",
                        cuenca ==  "CUENCA AUSTRAL ON-SHORE" ~ "AUSTRAL",
                        cuenca ==  "CUENCA CUYANA" ~ "CUYANA",
                        cuenca =="CUENCA GOLFO SAN JORGE" ~ "GOLFO SAN JORGE",
                        cuenca =="CUENCA NEUQUINA" ~ "NEUQUINA",
                        cuenca =="CUENCA NOROESTE" ~ "NOROESTE",
                        cuenca ==  "TOTAL CUENCA"~ "OTRA" )) %>%
  left_join(tc, by =c("anio", "mes")) %>% 
  group_by(mes, anio, unidad_gas, cuenca) %>% 
  summarise(precio_interno_gas = mean(precio_interno_gas/tc, na.rm = T)) %>% 
  mutate(unidad_gas = "Mm3")

################
#precio de crudo
#################
precio_crudo = read_delim("data/precio_mercado_interno_crudo_cuenca.csv",
                          delim = ";") %>% 
  rename(anio = "AÑO", mes = "MES") %>% 
  mutate(unidad_crudo = "m3",         
         anio =zoo::na.locf(anio),
         fecha = as.Date(parse_date_time(paste0(anio, mes), orders = "ym"))) %>% 
  select(mes, anio, unidad_crudo, everything(.)) %>% #fecha
  pivot_longer(., cols = c("CUENCA AUSTRAL OFF-SHORE" , "CUENCA AUSTRAL ON-SHORE",  
                           "CUENCA CUYANA",  "CUENCA GOLFO SAN JORGE"  ,
                           "CUENCA NEUQUINA", "CUENCA NOROESTE", "TOTAL CUENCA"),
               names_to = "cuenca", values_to = "precio_interno_crudo") %>% 
  mutate( cuenca = case_when( 
    cuenca == "CUENCA AUSTRAL OFF-SHORE" ~ "AUSTRAL",
    cuenca ==  "CUENCA AUSTRAL ON-SHORE" ~ "AUSTRAL",
    cuenca ==  "CUENCA CUYANA" ~ "CUYANA",
    cuenca =="CUENCA GOLFO SAN JORGE" ~ "GOLFO SAN JORGE",
    cuenca =="CUENCA NEUQUINA" ~ "NEUQUINA",
    cuenca =="CUENCA NOROESTE" ~ "NOROESTE",
    cuenca ==  "TOTAL CUENCA"~ "OTRA" )) %>%
  group_by(mes, anio, unidad_crudo, cuenca) %>% 
  summarise(precio_interno_crudo = mean(precio_interno_crudo, na.rm = T)) 

##################
#precios futuros
#################
precios_fut <- read_excel("data/PET_PRI_FUT_S1_D.xls", 
                               sheet = "Data 1", skip = 2) %>% 
  # mutate(fecha = strptime(as.character(Date), "%d/%m/%Y"))
  # mutate(fecha = substr(as.character(Date, 0, 4 ) ))
  # mutate(fecha = as.character(Date  ) )
  mutate(fecha = substring(as.character(Date), 0,7),
         mes = ym(Date))# %>% 
  # group_by(fecha) %>% 
  # summarise_all(function(x) { mean(x, na.rm = T )} )  )
glimpse(precios_fut)


#################
# riesgo país
#################
riesgo_pais <- read_excel("data/Serie_Historica_Spread_del_EMBI.xlsx", 
                          skip = 1) %>% as.data.frame() %>% 
  mutate(anio = year(Fecha), mes = month(Fecha)) %>% 
  group_by(anio, mes) %>% 
  summarise(riesgo_pais = mean(Argentina, na.rm = T))


##################
# join inversiones / produccion  / precios mdo int / riesgo país
##################
#revisión de missings  
# sum(is.na(prod_area$areapermisoconcesion))
# sum(is.na(prod_area$areayacimiento))
# sum(is.na(inversiones$area_per_conc))
# sum(is.na(inversiones$Yacimiento))


#join de bases
join =super_data  %>%
# join =prod_area  %>%
  left_join(inversiones, by = c(
                              # "areayacimiento" = "yacimiento" , 
                              "areapermisoconcesion" = "area_per_conc" ,
                                "cuenca" = "cuenca", 
                                "anio" = "anio")) %>% 
  left_join(precio_crudo, by =c("cuenca" = "cuenca", "mes" = "mes", "anio" = "anio") ) %>%  
  left_join(precio_gas, by =c("cuenca" = "cuenca", "mes" = "mes", "anio" = "anio") ) %>% 
  left_join(riesgo_pais, by = c("anio", "mes")) %>% 
  select(-c(unidad_gas, unidad_crudo))
glimpse(join)
  

# right_join(prod_area, by = c("yacimiento" ="areayacimiento" ))  
# right_join(prod_area, by = c("area_per_conc" ="areapermisoconcesion" , "cuenca" = "cuenca")) %>% 
    

# unique(prod$cuenca)    

#revisión de na's
# sum(is.na(join$explotacion_millones_usd ))
# sum(is.na(join$area_per_conc ))
# sum(is.na(join$areapermisoconcesion ))

# unique(join$areapermisoconcesion)
# glimpse(join)

#####################
# Exportacion
#####################
#dataset join
write.csv(join , paste0("data/resultados/", name_dataset_export),row.names = F)

#dataset produccion
write.csv(super_data , paste0("data/resultados/", name_dataset_produccion),row.names = F)


        