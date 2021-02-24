#EXTREMADURA 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(367,371,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

unique(datos_raw$)
unique(datos_raw$Municipio)

#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw) 


#eliminar redundantes
rioja = datos_clean %>%
  mutate(provincia = case_when(comunidad == "MADRID" ~ "MADRID" , 
                               comunidad == "LA RIOJA" ~ "LA RIOJA",
                               comunidad == "NAVARRA" ~ "NAVARRA",
                               comunidad == "MURCIA" ~ "MURCIA",
                               T ~ provincia)) %>%
  filter(comunidad == "LA RIOJA") %>%
  filter(!is.na(orden)) 

check = navarra %>% group_by(registro) %>%
  arrange(registro, orden) %>%
  mutate(check = orden - lag(orden,1))

#checar con lookup
check_provincias = rioja %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 

CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(rioja, file.path("intermediate_data", exfile))

