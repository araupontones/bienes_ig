#NAVARRA 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(406,445,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

unique(datos_raw$Municipio)
unique(datos_raw$Municipio)


#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw) 


#eliminar redundantes
navarra = datos_clean %>%
  mutate(provincia = "NAVARRA") %>%
  filter(provincia == "NAVARRA") %>%
  filter(!is.na(orden)) 
  


#checar con lookup
check_provincias = navarra %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)


unique(navarra$registro)

#exportar 

CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(navarra, file.path("intermediate_data", exfile))

