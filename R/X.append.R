source("set_up.R")


datos = list.files("intermediate_data")

read_intermediate = map(datos, function(x){
  
  file = file.path("intermediate_data", x)
  
  CA_datos = import(file)
  
  message(paste(x, ncol(CA_datos)))
  message(names(CA_datos))
  
  return(CA_datos)
  
  
})


intermediate_data = do.call(rbind, read_intermediate)

inter_clean = intermediate_data %>%
  mutate(Titular = case_when(Titular =='\"' ~ NA_character_, T ~ Titular)) %>%
  group_by(provincia, registro) %>%
  mutate(Titular = zoo::na.locf0(Titular)) %>%
  rename(dep_complementarias = `Templo y\r\ndependencias complementarias`,
         distito_de_cetificacion = `Título distinto de\r\ncertificación eclesiástica`) %>%
  select(-keep, -na_count) %>%
  filter(!distito_de_cetificacion %in% c("SI", "Si", "No", "NO") )


names(inter_clean) %>%
  filter(provincia == "OURENSE")


table(inter_clean$distito_de_cetificacion)
