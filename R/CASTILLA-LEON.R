#CANARIAS 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------
#273
tables = paste("Table", seq(310,313,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf1, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

unique(datos_raw$provincia)
names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


sort(unique(datos_clean$provincia))


#eliminar redundantes
leon = datos_clean %>%
  filter(comunidad == "CASTILLA-LEON") %>%
  filter(!is.na(orden)) %>%
  group_by(registro, orden) %>%
  mutate(seq = row_number()) %>%
  filter(!(provincia == "BURGOS" & seq ==2)) %>%
  select(-seq)





#checar con lookup
check_provincias = leon %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)


#exportar 
#
CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(leon, file.path("intermediate_data", exfile))

