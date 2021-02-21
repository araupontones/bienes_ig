source("set_up.R")


#read lookups 
look_ups = import(file.path(dir_lookups, "lookup.rds"))
provincias = look_ups$provincia
comunidades = "CATALUÑA"



pdf1 = file.path(dir_data, "pdf1.xlsx")
pdf2 = file.path(dir_data, "pdf2.xlsx")

test = read_xlsx(pdf2, sheet = "Table 405") 


test2 = test %>%
  as.tibble() %>%
  mutate(...7 = case_when(is.na(...7) ~"NA_", T~ ...7)) %>%
  mutate_all(as.character()) %>%
  mutate_if(is.character, function(x){ case_when(x=="NA" ~ NA_character_, T ~ x)}) %>%
  mutate(Comunidad = names(.)[1]) %>%
  rename(reference = 1) %>%
  mutate(detect_provincia= reference %in% provincias,
         provincia = case_when(detect_provincia ~ reference,
                               T ~ NA_character_),
         provincia = zoo::na.locf0(provincia),
         detect_registro = str_detect(reference, "[a-z]"),
         registro = case_when(!detect_registro & !reference %in% comunidades ~ reference,
                              T ~ NA_character_)
  )
 
test2 = test %>%
  mutate_all(as.character()) %>%
  mutate_if(is.character, function(x){ case_when(x=="NA" ~ NA_character_, T ~ x)})



transform_data = function(excel, sheet= "Table 1"){
  
  
  
  raw_data = read_xlsx(excel, sheet = sheet)
  
  if(ncol(raw_data<7)){
    
    raw_data = raw_data %>%
      mutate(...7 = NA)
  }
    raw_data = raw_data %>%                   
    as.tibble() %>%
    mutate(...7 = case_when(is.na(...7) ~"NA_", T~ as.character(...7))) %>%
    mutate_all(as.character()) %>%
    mutate_if(is.character, function(x){ case_when(x=="NA" ~ NA_character_, T ~ x)}) %>%
    mutate(Comunidad = names(.)[1]) %>%
    rename(reference = 1) %>%
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
            table = sheet
           
            ) %>%


     relocate(table, Comunidad, provincia, registro, localidad, na_count) %>%
    select(-reference) %>%
    select_if(function(x){!all(is.na(x))})
    
  
    if(!"provincia" %in% names(raw_data)) {
      
      raw_data = raw_data %>%
        mutate(provincia = NA) 
      
      
    }
    
  
  if(!"registro" %in% names(raw_data)) {
    
    raw_data = raw_data %>%
      mutate(registro = NA) 
    #%>%
    #relocate(table, Comunidad, provincia, registro, localidad, na_count)
    
  }
   
  
  
  message(paste(sheet, "columnas:", ncol(raw_data)))
  
  
  if(ncol(raw_data) < 15){
    
    
    raw_data = NULL
  } else{
    
   
    raw_data = raw_data %>%
      relocate(table, Comunidad, provincia, registro, localidad, na_count)
    message(names(raw_data))
    
    names(raw_data) <- paste0("col_", seq(1,ncol(raw_data)))
    
    return(raw_data)
  }
  
  
  
  
  
  
  
}

tables = paste("Table", seq(1,452,1))
tables_vascos= paste("Table", seq(387,452,1))

#3-------------------------------------------------

tables= paste("Table", seq(444,444,1))

datos_raw = map2(pdf2, tables_vascos, transform_data) %>%
  do.call(plyr::rbind.fill, .)

datos23_clean = datos_raw %>%
  rename(comunidad = col_2,
         provincia = col_3,
         registro= col_4,
         localidad = col_5,
         na_count = col_6,
         orden = col_7,
         municipio = col_8, 
         titulo = col_9,
         tipo = col_10) %>%
  
  mutate(
    
    provincia = zoo::na.locf0(provincia),
    registro = case_when(registro == "REGISTRO" ~ NA_character_, T ~ registro),
    registro = zoo::na.locf0(registro),
    provincia = case_when(comunidad == "MADRID" ~ "MADRID" , 
                          comunidad == "LA RIOJA" ~ "LA RIOJA",
                          comunidad == "NAVARRA" ~ "NAVARRA",
                          comunidad == "MURCIA" ~ "MURCIA",
                          T ~ provincia),
    
  ) %>%
  mutate(titulo2= case_when(is.na(lag(orden,1)) & is.na(lead(orden,1)) & !is.na(orden) ~ paste(lag(titulo,1), titulo, lead(titulo,1)),
                            T ~ titulo)) %>%
  add_count(registro,titulo, orden, name = "dup") %>%
  relocate(dup)  %>%
  mutate(drop = case_when(str_detect(localidad, "^TOTAL|^REGISTRO") & !comunidad %in% c("NAVARRA") ~ T,
                          #str_detect(titulo, "Suma Título consta") ~T,
                          
                          #registro == "SI" ~ T,
                          titulo %in% c("Titulo", "Tipo", "Suma Título consta", "Municipio")  ~ T,
                          
                          tipo %in% c("Titulo", "Tipo", "Suma Título consta") ~ T,
                          
                          municipio == "Municipio" & !comunidad %in% c("NAVARRA")~ T,
                          #str_detect(orden, "Orden") ~ T,
                          #str_detect(registro, "TOTAL|^NO") ~ T,
                          #comunidad %in% c("CATALUÑA", "NAVARRA", "MURCIA") & na_count > 5 ~ T,
                          comunidad %in% c("PAIS VASCO") & na_count > 4 ~ T,
                          #comunidad %in% c("NAVARRA") & na_count > 2 ~ T,
                          comunidad == "MURCIA" & na_count %in% c(7,10) ~ T,
                          
                          T~ F
  )) %>%
  relocate(drop) %>%
  filter(!drop)  %>%
  relocate(titulo2, .after = "titulo")




  
lleida = datos23_clean %>%
  filter(comunidad == "MURCIA") 




unique(lleida$tipo)
unique(lleida$orden)
table(lleida$col_13)
unique(lleida$col_1)

check_provincias = datos23_clean %>%
  group_by(provincia) %>%
  count() %>%
  left_join(look_ups) %>%
  mutate(dif = n - as.numeric(`NOTAS SIMPLES`)) %>%
  relocate(`NOTAS SIMPLES`,n, dif)




provincias
