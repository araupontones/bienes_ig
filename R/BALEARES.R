#ASTRURIAS 
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(179,188,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf1, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


sort(unique(datos_clean$provincia))

#eliminar redundantes
baleares = datos_clean %>%
  mutate(provincia = case_when(comunidad == "BALEARES" ~ "BALEARES" , 
                               comunidad == "LA RIOJA" ~ "LA RIOJA",
                               comunidad == "NAVARRA" ~ "NAVARRA",
                               comunidad == "MURCIA" ~ "MURCIA",
                               T ~ provincia)) %>%
  filter(comunidad == "BALEARES") %>%
  #filter(na_count < 4) %>%
  filter(!is.na(orden)) %>%
  group_by(Municipio, registro, orden, Titulo) %>%
  slice(1) 


unique(asturias$registro)




#checar con lookup
check_provincias = baleares %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 
#
CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(baleares, file.path("intermediate_data", exfile))

