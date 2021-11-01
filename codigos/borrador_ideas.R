rm(list=ls())
gc()

library(tidyverse)
library(readr)
library(lubridate)


#pozos
pozos = read_csv("data/listado-de-pozos-cargados-por-empresas-operadoras.csv")
glimpse(pozos)

table(pozos$formprod )

#produccion 
prod_18  = read_csv("data/prod_capiv/produccin-de-pozos-de-gas-y-petrleo-2018.csv")
glimpse(prod_18)

unique(prod_18$cuenca)

prod_18_sel = prod_18 %>% 
  select( idempresa , anio, mes, 
         idareapermisoconcesion , areapermisoconcesion ,
         idareayacimiento , areayacimiento , 
         cuenca, provincia,
         prod_pet, prod_gas, prod_agua,
         tef  , tipoextraccion , 
         tipoestado ,  tipopozo, subclasificacion ,    formprod ,
         tipo_de_recurso , proyecto , clasificacion, sub_tipo_recurso  )

table(prod_18$proyecto     )
table(prod_18$clasificacion )
table(prod_18$tipo_de_recurso )
table(prod_18$tipoextraccion )
table(prod_18$proyecto )


unique(prod_18$formacion)


prod_18_area = prod_18 %>% 
  filter(clasificacion == "EXPLOTACION" ) %>% 
  mutate(cuenca = case_when(cuenca %in% c( "TOTAL CUENCA" ,"CAÑADON ASFALTO", 
                                           "ÑIRIHUAU" , NA  ) ~ "OTRA",
                            T ~cuenca)) %>% 
  # mutate(idareapermisoconcesion = as.double(idareapermisoconcesion)) %>% 
  group_by(anio, mes, cuenca ,areayacimiento,  areapermisoconcesion ) %>% 
  # group_by(areayacimiento , idareayacimiento) %>% 
  summarise(prod_pet = sum(prod_pet, na.rm = T), 
            prod_gas = sum(prod_gas, na.rm = T),
            prod_agua = sum(prod_agua, na.rm = T)) 
glimpse(prod_18_area)

# modelo random
model = lm(formula = prod_pet ~  profundidad+ tipo_de_recurso , data = prod_18)
summary(model)


#inversiones
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
  filter(anio == 2018 )# %>% 
  # group_by(cuenca, area_per_conc) %>% 
  # summarise(inversion_explotacion = sum(explotacion_millones_usd, na.rm = T),
  #           inversion_exploracion = sum(exploracion_millones_usd, na.rm = T))

glimpse(inversiones)
summary(inversiones)


# precio
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
  select(-fecha)
  
precio_crudo = read_delim("data/precio_mercado_interno_crudo_cuenca.csv",
                          delim = ";") %>% 
  rename(anio = "AÑO", mes = "MES") %>% 
  mutate(unidad_crudo = "USD/m3",         
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
  select(-fecha)
  
glimpse(precio_crudo)  

# join inversiones / produccion  
  
sum(is.na(prod_18_area$areapermisoconcesion))
sum(is.na(prod_18_area$areayacimiento))

sum(is.na(inversiones$area_per_conc))
sum(is.na(inversiones$Yacimiento))


join =prod_18_area  %>%
  left_join(inversiones, by = c(
                              "areayacimiento" = "yacimiento" , 
                              "areapermisoconcesion" = "area_per_conc" , 
                                "cuenca" = "cuenca", 
                                "anio" = "anio")) %>% 
  left_join(precio_crudo, by =c("cuenca" = "cuenca", "mes" = "mes", "anio" = "anio") ) %>%  
  left_join(precio_gas, by =c("cuenca" = "cuenca", "mes" = "mes", "anio" = "anio") ) 

  
join[is.na(join$inversion_explotacion),]  
  # right_join(prod_18_area, by = c("yacimiento" ="areayacimiento" ))  
    # right_join(prod_18_area, by = c("area_per_conc" ="areapermisoconcesion" , "cuenca" = "cuenca")) %>% 
    

unique(prod_18$cuenca)    


sum(is.na(join$explotacion_millones_usd ))
sum(is.na(join$area_per_conc ))

glimpse(join)

lm()

        