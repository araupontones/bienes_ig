source("set_up.R")


#read lookups 
look_ups = import(file.path(dir_lookups, "lookup.rds"))
provincias = look_ups$provincia
comunidades = "CATALUÑA"



pdf1 = file.path(dir_data, "pdf1.xlsx")
pdf2 = file.path(dir_data, "pdf2.xlsx")



##crear nombre de columnas -----------------------------------------------------
test = read_xlsx(pdf2, sheet = "Table 14") 

names <- as.character(test[1,])
fuera = which(names %in% c(NA,'NA'))
names = names[-fuera]



#funcion para importar tablas --------------------------------------------------
importar_tablas= function(excel, sheet){
  
  
  raw_excel = read_xlsx(pdf2, sheet = sheet) 
  
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
    
    #agregar nombre de la comunidad, numero de tabla y numero de columnas de la tabla
    add_comunidad = drop_empty_col %>%
      mutate(table = sheet,
             cols = ncol(.),
             comunidad = CA) %>%
      rename(reference = 1) %>%
      relocate(table, cols, comunidad, reference)
    
    
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
             na_count = apply(., 1, function(x) sum(is.na(x))),
             
      ) %>%
      #eliminar observaciones vacias
      filter(na_count <8) %>%
      select(-reference, - detect_provincia, -detect_registro) %>%
      relocate(table, comunidad, provincia, registro, localidad, na_count)
    
   
    
    return(add_geo)
    
  }
  
  
}


#leer tablas --------------------------------------------------------------------------------------------
tables = paste("Table", seq(446,451,1))
datos_raw = map2(pdf2, tables, importar_tablas) %>% do.call(plyr::rbind.fill,.)



#limpiar datos ----------------------------------------------------------------------
datos_clean = datos_raw %>%
  mutate(keep = case_when(`Nº Orden` == "0" ~ F,
                          Titulo == "Titulo" ~ F,
                          str_detect(registro, "^TOTAL") ~ F,
                          str_detect(Municipio,"^Suma Título consta") ~ F,
                          str_detect(localidad, "^TOTAL") ~ F,
                          Municipio == "Municipio" ~ F,
                          T ~ T)
         ) %>%
  filter(keep) %>%
  mutate(provincia = zoo::na.locf0(provincia),
         registro = case_when(registro == "REGISTRO" ~ NA_character_, T ~ registro),
         registro = zoo::na.locf0(registro),
         localidad = case_when(str_detect(localidad,"^stop|^Nº") ~ NA_character_, T ~ localidad),
         localidad = zoo::na.locf0(localidad)
         ) %>%
  rename(orden = `Nº Orden`) %>%
  mutate(across(c(Municipio, Titular), function(x){
    case_when(!is.na(lag(orden,1)) & !is.na(orden) & is.na(lead(orden,1)) & !is.na(lead(orden,2)) ~ 
                paste(x, lead(x,1)),
              T ~ x
    )
    
  }))
           
           
         
         
# crear tablas por comunidad autonoma ------------------------------------------

#Extremadura listo!
extremadura = datos_clean %>%
  filter(comunidad == "EXTREMADURA") %>%
  filter(!is.na(orden))



#Catalunya (LLeida tiene mas observaciones, Barcelona + 1) 
catalunia = datos_clean %>%
  filter(comunidad == "CATALUÑA") %>%
  filter(!is.na(orden)) %>%
  group_by(provincia,registro, localidad, orden, Titulo, na_count) %>%
  slice(1) 


  
# Galicia(fCoruna +8)
galicia = datos_clean %>%
  filter(comunidad == "GALICIA") %>%
  filter(!is.na(orden)) %>%
  add_count(provincia,registro, localidad, orden, Titulo, na_count) %>%
  mutate(save = case_when(n>2 ~ paste(localidad, table), T~ localidad)) %>%
  select(-n) %>%
  group_by(provincia,registro, localidad, save, orden, Titulo, na_count) %>%
  slice(1) %>%
  ungroup() %>%
  select(-save)
 
 
#vascos (LISTOS)
vascos = datos_clean %>%
  filter(comunidad == "PAIS VASCO") %>%
  filter(!is.na(orden)) 


datos_tot = rbind(catalunia, extremadura, galicia, vascos)


check_provincias = vascos %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)
