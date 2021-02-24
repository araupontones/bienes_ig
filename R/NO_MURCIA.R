#MURCIA 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(387,404,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)


#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


unique(datos_clean$localidad)


#eliminar redundantes
murcia = datos_clean %>%
  mutate(provincia =  "MURCIA") %>%
  filter(comunidad == "MURCIA") %>%
  filter(!is.na(orden)) %>%
  group_by(registro, orden) %>%
  slice(1) 

names(murcia)




#checar con lookup
check_provincias = murcia %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 

CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(murcia, file.path("intermediate_data", exfile))

