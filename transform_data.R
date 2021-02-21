source("set_up.R")






#Funcion para importar datos --------------------------------------------------

format_excel = function(excel, x){
  
  message(x)
  
  table_raw= read_xlsx(excel, sheet = x)
  
  ##to overcome the issue of multiple rows by obs. (*TEST, puede ser que no valga la pena)
  if(x %in% tables_galicia & excel == "pdf2.xlsx"){
    
    table_raw = table_raw %>%
      #fill municipio
      mutate(...3 = zoo::na.locf0(...3)) %>%
      #by orden y municipio
      group_by(...2, ...3) %>%
      #colapsar titulo por orden
      mutate(...4 = paste0(...4, collapse = " ")) %>%
      ungroup() %>%
      filter(!is.na(.[[1]]))
    
    
  }
  
  ## Estas tablas solamente tienen sellos
  if(x %in% c("Table 385")){
    table_clean = NULL
    
  } else {
  
  #limpiar raw sheet
  table_clean = table_raw %>%
    mutate(
      ##comunidad autonoma
      CA = names(.)[1],
      ##identificar tabla del excel
      table = x) %>%
    filter(!row_number()==1) %>%
    
    ##limpiar sellos que aparecen de manera aleatoria
    mutate_if(is.character,function(x){case_when(x=="16 FEB. 2" ~ NA_character_,
                                                 T ~ x)}) %>%
    ##intento por crear localidad (*A MEJORAR)
    mutate(localidad = case_when(.[[1]]=='"' ~ NA_character_,
                                 T ~ .[[1]]),
           localidad = zoo::na.locf0(localidad),
           localidad = case_when(all(is.na(localidad)) ~ "NA_",
                                 T ~ localidad
           )
    ) %>%
    ##eliminar columnas vacias
    select_if(function(x) !(all(is.na(x)) | all(x==""))) %>%
    select(-1) 
  
  
  ##Si la tabla tiene mas de 11 columnas
  if(ncol(table_clean)>11){
    
    until_col = which(names(table_clean)=="CA") - 3
    
    table_clean = table_clean %>%
      select(-c(7:until_col))
    
    print(ncol(table_clean))
  
    
  }
  
  
  ##saltar estos pasos si es una tabla resumen
  if(ncol(table_clean ) ==11) {
    
    table_clean = table_clean %>%
      rename(Orden = 1,
             Municipio = 2,
             Titulo = 3,
             Tipo = 4,
             Dependencias = 5,
             Titular = 6,
             Certificacion = 7,
             Total = 8) %>%
      filter(!is.na(Orden)
             #!is.na(Municipio)
             ) %>%
      
      relocate(table, CA, localidad)
    
    return(table_clean)  
    
    
  } else {
    print(ncol(table_clean))
    table_clean = NULL
    
  }
  
  
  }
  
}




#Importar datos -------------------------------------------------------------------------

tables_galicia = paste("Table", seq(57,366,1))

tables = paste("Table", seq(19,313,1))
tables2 = paste("Table", seq(1,452,1))





mi_lista = map2("pdf1.xlsx",tables, format_excel)
#mi_lista2 = map2("pdf2.xlsx",tables2, format_excel)



mis_datos = do.call(rbind, mi_lista) %>% tibble()
mis_datos2 = do.call(rbind, mi_lista2) %>% tibble() 




##limpiar (ESTO NECESITA MAS TRABAJO Y TIEMPO) ----------------------------------

mis_datos_clean = mis_datos %>%
  rbind(mis_datos2) %>%
  mutate(localidad = case_when(localidad ==  "NA_" ~ NA_character_,T ~ localidad),
         localidad = zoo::na.locf0(localidad)) %>%
  ##elimino localidad porque resulto muy sucia (MEJORAR EN LA FUNCION)
  select(
         -Total) %>%
##eliminar filas que son titulos de tabla pero ingresaron como observacion (4 obs)
filter(Orden != "Nº Orden") %>%
  ##eliminar observaciones vacias (a causa del formato del excel)
  mutate_all(function(x){case_when(x == "Suma Título consta" ~ NA_character_,
                                   T ~ x)}) %>%
  mutate(na_count = apply(., 1, function(x) sum(is.na(x))),
         drop = case_when(
                          
                          CA %in% c("BALEARES", "CATALUÑA") & na_count >5 ~ T,
                          
                          CA %in% c("ARAGON", "CASTILLA-LA MANCHA", "NAVARRA", 'MURCIA') & na_count > 4 ~ T,
                          
                          CA %in% c("ANDALUCIA OCCIDENTAL", "ANDALUCIA ORIENTAL", 
                                    "ASTURIAS", "CANARIAS-LAS PALMAS",
                                    "COMUNIDAD VALENCIANA", 'MADRID') & na_count > 3 ~ T,
                          
                          CA %in% c("PAIS VASCO", "GALICIA", "LA RIOJA") & na_count > 2 ~ T,
                    
                          
                          
                          CA == "ARAGON" & str_detect(Tipo, "a47|r947|7rroquial") ~ T ,
                          
                          CA == "BALEARES" & Titulo == "en" ~ T,
                          T ~ F)
         
         ) %>%
  filter(!drop)


Tipo = mis_datos_clean %>% count(Tipo)




export(select(mis_datos_clean,-drop), "bienes_ig_transformados.rds")



## check: ver observaciones por CA ----------------------------------------------------------------------


#comunidades verificadas
checked_CA= c("ANDALUCIA OCCIDENTAL", "ANDALUCIA ORIENTAL", "ASTURIAS",
              "BALEARES", 'CANARIAS-LAS PALMAS', "CANTABRIA", 'CASTILLA-LA MANCHA',
              "COMUNIDAD VALENCIANA", 'LA RIOJA', 'PAIS VASCO')

mis_datos_clean %>%
  group_by(CA) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(CA = factor(CA,
                     levels = rev(.$CA)),
         status = case_when(CA %in% checked_CA ~ "== a pdf",
                           T ~ "!= a pdf")) %>%
         
ggplot(data = .,
       aes(x=n,
           y = CA,
           label = prettyNum(n, big.mark = ','),
           fill = status) 
       ) +
  geom_col() +
  geom_text(hjust = 0, size = 3)
  #xlim(0,10600)


