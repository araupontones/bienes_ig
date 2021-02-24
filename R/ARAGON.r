#ARAGON 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(124,173,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf1, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)
unique(datos_raw$localidad)
names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


sort(unique(datos_clean$registro))

#eliminar redundantes
aragon = datos_clean %>%
  filter(comunidad == "ARAGON") %>%
  mutate(registro = case_when(registro == "MURCIA" ~ NA_character_, T ~ registro),
         registro = zoo::na.locf0(registro),
         localidad = case_when(str_detect(localidad, "REGISTRO") ~ NA_character_, T ~ localidad),
         localidad = zoo::na.locf0(localidad)) %>%
  filter(na_count < 4) %>%
  filter(!is.na(orden)) 

unique(aragon$provincia)



#checar con lookup
check_provincias = aragon %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 
#
CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(aragon, file.path("intermediate_data", exfile))

