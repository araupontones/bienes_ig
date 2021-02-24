#GALICIA

if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}



#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(57,365,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

datos_raw2 = datos_raw %>%
  mutate(across(c(Titulo,
                  `Templo y\r\ndependencias complementarias`), 
                function(x){case_when(x == "na"~ NA_character_, T ~ x)}),
         Municipio = str_remove(Municipio, "\r\n16 FEB. 2|16 FEB. 2"),
         Titulo = str_remove_all(Titulo, "021 13:12:33 Ent"),
         Municipio = str_trim(Municipio)
         ) 



datos_raw[str_detect(datos_raw2$Titulo, "021 13:12:33 Ent"),1]

sort(unique(datos_raw$Municipio))
sort(unique(datos_raw$registro))

#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw2)




#eliminar redundantes
galicia = datos_clean %>%
  filter(comunidad == "GALICIA") %>%
  #mutate(Titulo = str_remove(Titulo, "021 13:12:33 Entrada: 88949")) %>%
  filter(!is.na(orden)) %>%
  add_count(provincia,registro, localidad, orden, Titulo, na_count) %>%
  group_by(registro, orden, Municipio, Titulo) %>%
  arrange(registro, orden, Titulo, Municipio, localidad) %>%
  group_by(registro, orden, Titulo, Municipio) %>%
  slice(1) %>%
  select(-n) %>%
  ungroup() %>%
  filter(!table %in% c("Table 282", "Table 297")) %>%
  filter(!(table == "Table 69" & orden ==332)) %>%
  filter(!(table == "Table 224" & orden ==72))
  
  #%>%
  #add_count(registro, orden) %>%
  #filter(n >1)

#checar con lookup
check_provincias = galicia %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)

#exportar 

CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(galicia, file.path("intermediate_data", exfile))

