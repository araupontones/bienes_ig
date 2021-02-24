#CATALUNIA (en lleida no me da la suma)
if(!exists("look_ups", envir = globalenv())){
  
  source("set_up.R")
  
}



#definir tablas --------------------------------------------------------------

tables = paste("Table", seq(1,29,1))

#leer tablas --------------------------------------------------------------------------------------------
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)

names(datos_raw)
#limpiar datos  --------------------------------------------------------------
datos_clean = limpar_datos(datos_raw)

unique(datos_clean$Municipio)

#eliminar redundantes
cataluna = datos_clean %>%
  filter(comunidad == "CATALUÑA") %>%
  filter(!is.na(orden)) %>%
  group_by(registro, orden) %>%
  slice(1) 
  #delete duplicate Barcelona
  filter(!(registro == "VENDERELL, EL Nº 02" & Titulo == "Esglesia de San Andreu i Santa Marina" )) %>%
  mutate(Titular = case_when(Titular == '"' ~ NA_character_ , T ~ Titular),
                 Titular = zoo::na.locf0(Titular))


  sort(unique(cataluna$registro))








#checar con lookup
check_provincias = cataluna %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)

no_relevante = c("Pieza de tierra", "pieza de tierra", "Sin nombre *", "FINCA RUSTICA", "Paratge Solana",
                 "Paratge Cultia", "PARAJE BASTANIST") 

check = cataluna %>%
  filter(provincia== "LLEIDA") %>%
  filter(!Titulo %in% no_relevante) %>%
  add_count(registro, Titulo,Municipio ,Titular) %>%
  arrange(desc(n)) %>%
  filter(n >1)


#exportar 

CA = str_replace(datos_clean$comunidad[1], " ", "_")
exfile = paste0(CA,".rds")

export(cataluna, file.path("intermediate_data", exfile))

