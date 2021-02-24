#Pais vasco


if(!exists("names", envir = globalenv())){
  
  source("set_up.R")
  
}



#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(446,451,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)


#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)


#eliminar redundantes
vascos = datos_clean %>%
  filter(comunidad == "PAIS VASCO") %>%
  filter(!is.na(orden)) 


#checar con lookup
check_provincias = vascos %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)

#exportar 

CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(vascos, file.path("intermediate_data", exfile))

