source("set_up.R")




#Funcion para importar datos --------------------------------------------------

format_excel = function(excel, x){
  
  message(x)
  
  table_raw= read_xlsx(excel, sheet = x)
  
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




#Importar datos -------------------------------------------------------------------------

tables = paste("Table", seq(19,313,1))


mi_lista = map2("pdf1.xlsx",tables, format_excel)

mis_datos = do.call(rbind, mi_lista) %>% tibble()



##limpiar (ESTO NECESITA MAS TRABAJO Y TIEMPO) ----------------------------------

mis_datos_clean = mis_datos %>%
  ##elimino localidad porque resulto muy sucia (MEJORAR EN LA FUNCION)
  select(-localidad,
         -Total) %>%
##eliminar filas que son titulos de tabla pero ingresaron como observacion (4 obs)
filter(Orden != "Nº Orden") %>%
  ##eliminar observaciones vacias (a causa del formato del excel)
  mutate_all(function(x){case_when(x == "Suma Título consta" ~ NA_character_,
                                   T ~ x)}) %>%
  mutate(na_count = apply(., 1, function(x) sum(is.na(x))),
         drop = case_when(CA %in% c("ANDALUCIA OCCIDENTAL", "ANDALUCIA ORIENTAL", 
                                    "ASTURIAS", "CANARIAS-LAS PALMAS") & na_count > 3 ~ T,
                          CA %in% c("BALEARES") & na_count >5 ~ T,
                          CA %in% c("ARAGON", "CASTILLA-LA MANCHA") & na_count > 4 ~ T,
                          CA %in% c("CANARIAS-TENERIFE", "CASTILLA-LEON",
                                    "COMUNIDAD VALENCIANA") & na_count > 3 ~ T,
                          CA == "ARAGON" & str_detect(Tipo, "a47|r947|7rroquial") ~ T ,
                          CA == "BALEARES" & Titulo == "en" ~ T,
                          T ~ F)
         
         ) %>%
  filter(!drop)



export(mis_datos_clean, "andalucia_castillaLeon.rds")
# c_titular = mis_datos_clean %>%
#   filter(is.na(Titular))


## check: ver observaciones por CA ----------------------------------------------------------------------

check_data = mis_datos_clean %>%
  group_by(CA) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(CA = factor(CA,
                     levels = rev(.$CA)))
         
ggplot(data = check_data,
       aes(x=n,
           y = CA,
           label = prettyNum(n, big.mark = ',')) 
       ) +
  geom_col() +
  geom_text(hjust = 0)+
  xlim(0,10600)


