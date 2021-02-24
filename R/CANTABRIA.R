#CANARIAS 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------
#190
tables = paste("Table", seq(271,271,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf1, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


sort(unique(datos_clean$provincia))



#eliminar redundantes
cantabria = datos_clean %>%
  mutate(provincia = "CANTABRIA") %>%
  filter(comunidad == "CANTABRIA") %>%
  filter(!is.na(orden))


unique(tenerife$Municipio)





#checar con lookup
check_provincias = cantabria %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 
#
CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(cantabria, file.path("intermediate_data", exfile))

