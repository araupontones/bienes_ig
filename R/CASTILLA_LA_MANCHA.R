#la mancha 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------
#273
tables = paste("Table", seq(273,308,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf1, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

unique(datos_raw$provincia)
names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


sort(unique(datos_clean$provincia))



#eliminar redundantes
castilla = datos_clean %>%
  filter(comunidad == "CASTILLA-LA MANCHA") %>%
  filter(!is.na(orden))


unique(tenerife$Municipio)





#checar con lookup
check_provincias = castilla %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 
#
CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(castilla, file.path("intermediate_data", exfile))

