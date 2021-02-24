#ANDLUCIA 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(19,123,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf1, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


unique(datos_clean$registro)

#eliminar redundantes
andalucia = datos_clean %>%
  filter(str_detect(comunidad, "ANDALUCIA")) %>%
  mutate(registro = case_when(registro == "MURCIA" ~ NA_character_, T ~ registro),
         registro = zoo::na.locf0(registro),
         localidad = case_when(str_detect(localidad, "REGISTRO") ~ NA_character_, T ~ localidad),
         localidad = zoo::na.locf0(localidad)) %>%
  filter(na_count < 5) %>%
  filter(!is.na(orden)) %>%
  filter(!registro %in% c("NO","SI")) %>%
  filter(!(Municipio == "50" & orden == 723))
  
  group_by(registro, orden) %>%
  slice(1) 



#checar con lookup
check_provincias = andalucia %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 
#datos_clean$comunidad[1]
CA = str_replace("ANDALUCIA", " ", "_")
exfile = paste0(CA,".rds")

export(andalucia, file.path("intermediate_data", exfile))

