source("set_up.R")


datos = list.files("intermediate_data")

read_intermediate = map(datos, function(x){
  
  file = file.path("intermediate_data", x)
  
  CA_datos = import(file)
  
  message(paste(x, ncol(CA_datos)))
  message(names(CA_datos))
  
  return(CA_datos)
  
  
})

Sys.getlocale()
Sys.setlocale("LANG"="Spanish")

intermediate_data = do.call(rbind, read_intermediate)

inter_clean = intermediate_data %>%
  mutate(Titular = case_when(Titular =='\"' ~ NA_character_, T ~ Titular)) %>%
  group_by(provincia, registro) %>%
  mutate(Titular = zoo::na.locf0(Titular)) %>%
  ungroup() %>%
  rename(dep_complementarias = `Templo y\r\ndependencias complementarias`,
         distinto_de_cetificacion = `Título distinto de\r\ncertificación eclesiástica`) %>%
  mutate(distinto_de_cetificacion = case_when(localidad == "Ventosa del Río Almar" &
                                                 orden %in% c(62, 63) ~ "NO",
                                               T ~ distinto_de_cetificacion)) %>%
  select(-keep, -na_count) 





## CHECK PROVINCES 

provincias = inter_clean %>%
  group_by(provincia) %>%
  summarise(`NOTAS SIMPLES TRANSF` = n(),
            `Titulo distinto de certificacion eclesiastica == NO` = sum(distinto_de_cetificacion=="NO", na.rm = T),
            `Titulo distinto de certificacion eclesiastica == SI` = sum(distinto_de_cetificacion=="SI", na.rm = T),
            .groups = 'drop'
            ) 

provincias_check = look_ups %>%
  left_join(provincias) %>%
  mutate(Diferencia = `NOTAS SIMPLES TRANSF` -as.numeric(`NOTAS SIMPLES`) ) %>%
  select(-`REGISTROS INTRODUCIDOS`) %>%
  relocate(Diferencia)  



unique(inter_clean$distinto_de_cetificacion)

provincias = rev(sort(unique(provincias_check$provincia)))

provincias_check$provincia <- factor(provincias_check$provincia, levels = provincias)

provincias

ggplot(data = provincias_check,
       aes(x = Diferencia,
           y = provincia)) +
  geom_col() +
  geom_vline(xintercept = 0) +
  geom_text(data = subset(provincias_check, Diferencia >0),
            aes(label = Diferencia),
            hjust = 0,
            nudge_x =1) +
  
  geom_text(data = subset(provincias_check, Diferencia < 0),
            aes(label = Diferencia),
            hjust = 1,
            nudge_x =-1) +
  #xlim(-15,90) +
  scale_x_continuous(labels = seq(-15,90,5),
                     breaks = seq(-15,90,5)) +
  labs(title = "Diferencia entre datos en documento y datos trasformados",
       subtitle = "Negativo significa que existen menos observaciones en los datos transformados")




ggsave(last_plot(),file = "datos_final/resumen_de_transformacion.png",
       height = 30,
       units = 'cm')



#export data
exit_folers = c("datos_final", "app")
map(exit_folers, function(x){
  
  exdir = file.path(x,"bienes_iglesia.rds")
  export(inter_clean,exdir )
  export(provincias_check, file.path(x,"resultado_formato.rds"))
  
})  



##crear lookup tables para app

comunidades_lkp = sort(unique(inter_clean$comunidad)) %>% export(.,"app/comunidades_lkp.rds" )


provincias_lkp = inter_clean %>%
  group_by(comunidad, provincia) %>%
  select(comunidad, provincia) %>%
  slice(1) %>%
  arrange(comunidad, provincia) %>% export(.,"app/provincias_lkp.rds" )


xlsx::write.xlsx(provincias_check,"datos_final/resumen_transformacion.xlsx" )

gpi_table_clean%>%
  slice(1) %>%
  pull(var =ANDALUCIA.OCCIDENTAL )
