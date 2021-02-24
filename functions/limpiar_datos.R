
#Funcion para limiapr datos

edificio = "^ATRIO|^Atrio|^Capilla|^Casa|^EDIFICIO|^Edificio|^Ermita|^Huerto|^Iglesia|^IGLESIA|^Parcela|^Pastos|^TEMPLO|^Templo|^Solar"

limpar_datos = function(db){
  
  
  datos_clean = db %>%
    rename(orden = `Nº Orden`) %>%
    mutate(keep = case_when(orden == "0" ~ F,
                            Titulo == "Titulo" ~ F,
                            str_detect(registro, "^TOTAL") ~ F,
                            str_detect(Municipio,"^Suma Título consta") ~ F,
                            str_detect(localidad, "^TOTAL") ~ F,
                            Municipio == "Municipio" ~ F,
                            T ~ T)
    ) %>%
    filter(keep) %>%
    mutate(provincia = zoo::na.locf0(provincia),
           registro = case_when(registro %in% c("REGISTRO","CATALUÑA", "ANDALUCIA OCCIDENTAL", 'SI', 'ARAGON', 'MURCIA')  ~ NA_character_, 
                                str_detect(registro, " 0$") ~ NA_character_,
                                T ~ registro),
           registro = zoo::na.locf0(registro),
           localidad = case_when(str_detect(localidad,"^stop|^Nº|88949| Título|Suma Título|Entrada|^REGISTRO|^Relación de aquellas|^16 FEB. 2") ~ NA_character_, T ~ localidad),
           localidad = zoo::na.locf0(localidad)
    ) %>%
    mutate(across(c(Municipio, Titular, Titulo), function(x){
      case_when(!is.na(lag(orden,1)) &
                  !is.na(orden) &
                  is.na(lead(orden,1)) & !is.na(lead(x,1)) &
                  !is.na(lead(orden,2))  ~
                  paste(x, lead(x,1)),
                
                is.na(lag(orden,1)) &
                  !is.na(orden) &
                  is.na(lead(orden,1)) &!is.na(lead(x,1)) ~ paste(x, lead(x,1)),
                T ~ x)
      
    })
    
    
    
    ) %>%
    
    mutate(Titulo = case_when(
      is.na(lag(orden,2)) &  
      (str_detect(lag(Titulo,2), edificio) | is.na(lag(Titulo,2)) ) &
        is.na(lag(orden,1)) &
        !is.na(orden) &
        is.na(lead(orden,1)) &
        is.na(lead(orden,2)) &  
        !str_detect(lead(Titulo,2), edificio)  ~ 
        paste(lag(Titulo,2), lag(Titulo,1), Titulo, lead(Titulo,1), lead(Titulo,2) ),
      
      
      is.na(lag(orden,1)) &
        !is.na(orden) &
        is.na(lead(orden,1)) &
        (str_detect(lag(Titulo,1), edificio) | is.na(lag(Titulo,1)))
      ~ paste(lag(Titulo,1), Titulo, lead(Titulo,1)),
      
      is.na(lag(orden,1)) & 
        !is.na(orden) &
        is.na(lead(orden,1)) & !str_detect(lead(Titulo,1), edificio) ~ 
        paste(Titulo, lead(Titulo,1)),
      
      
      T ~ Titulo
      
    ),
    
    Titulo = str_remove_all(Titulo, "NA | NA$"),
    orden = as.numeric(orden)
    ) %>%
    relocate(na_count)
  
  
  
}
