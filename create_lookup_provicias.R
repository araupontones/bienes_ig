library(tabulizer)


source("set_up.R")


pdf1 = file.path(dir_data, "pdf1.xlsx")


tables = paste("Table",seq(2,5,1))

read_look_ups = function(table){
  
  read_xlsx(pdf1, sheet = table)
}

lookups = map(tables, read_look_ups) %>%
  do.call(plyr::rbind.fill, .)


lookups_clean = lookups %>%
  mutate(provincia = ...1,
         provincia=case_when(is.na(provincia) ~ BALEARES,
                    T~ provincia),
         provincia = case_when(is.na(provincia) ~ `CASTILLA-LEON`,
                              T~ provincia),
         provincia = case_when(is.na(provincia) ~ `LA RIOJA`,
                               T~ provincia)
  ) %>%
  relocate(provincia) %>%
  filter(!str_detect(provincia, "TOTAL")) %>%
  filter(is.na(`REGISTROS INTRODUCIDOS`)) %>%
  select(1, 3:6)


export(lookups_clean, file.path(dir_lookups, "lookup.rds"))
