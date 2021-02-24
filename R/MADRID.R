#MADRID (en lleida no me da la suma)
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}


#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(373,384,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)



#eliminar redundantes
madrid = datos_clean %>%
  mutate(provincia = case_when(comunidad == "MADRID" ~ "MADRID" , 
                               comunidad == "LA RIOJA" ~ "LA RIOJA",
                               comunidad == "NAVARRA" ~ "NAVARRA",
                               comunidad == "MURCIA" ~ "MURCIA",
                               T ~ provincia)) %>%
  filter(comunidad == "MADRID") %>%
  filter(!is.na(orden))
  





#checar con lookup
check_provincias = madrid %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)



#exportar 

CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(madrid, file.path("intermediate_data", exfile))

