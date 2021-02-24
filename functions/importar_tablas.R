

#funcion para importar tablas --------------------------------------------------
importar_tablas= function(excel, sheet){
  
  
  raw_excel = read_xlsx(excel, sheet = sheet) 
  
  message(sheet)
  message(paste("columnas primero:", ncol(raw_excel)))
  message(names(raw_excel))
  
  if(ncol(raw_excel)>7) {
    
    #tirar primera fila de datos (son los titulos)
    raw = raw_excel[-1, ]
    
    
    #identificac CA
    CA = names(raw_excel)[1]
    
    #eliminar columnas vacias
    drop_empty_col = raw %>%
      select_if(function(x) !(all(is.na(x)) | all(x==""))) 
    
    
    message(ncol(drop_empty_col))
    
    #definir nombres de las columnas
    names(drop_empty_col) <- names 
   message(names)
    
    #agregar nombre de la comunidad, numero de tabla y numero de columnas de la tabla
    add_comunidad = drop_empty_col %>%
      mutate(table = sheet,
             comunidad = CA) %>%
      rename(reference = 1) %>%
      relocate(table, comunidad, reference)
    
    
    
    #agregar provincia, registro y municipio
    add_geo = add_comunidad %>%
      mutate(detect_provincia= reference %in% provincias,
             provincia = case_when(detect_provincia ~ reference,
                                   T ~ NA_character_),
             provincia = zoo::na.locf0(provincia),
             detect_registro = str_detect(reference, "[a-z]"),
             registro = case_when(!detect_registro & !reference %in% comunidades ~ reference,
                                  T ~ NA_character_),
             registro = case_when(str_detect(registro, "[A-Z]")   ~ registro, T ~ NA_character_),
             registro = zoo::na.locf0(registro),
             
             localidad = case_when(registro == reference ~ "stop", T ~ NA_character_),
             localidad = case_when(is.na(localidad) & detect_registro ~ reference, T ~ localidad),
             localidad = zoo::na.locf0(localidad),
             na_count = apply(., 1, function(x) sum(is.na(x)))
             
             
      ) %>%
      #eliminar observaciones vacias
      #filter(na_count <6) %>%
      select(-reference, - detect_provincia, -detect_registro) %>%
      relocate(table, comunidad, provincia, registro, localidad)
    
  
    
    return(add_geo)
    
  }
  
  
}
