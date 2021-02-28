library(pacman)

p_load(readxl, tidyverse, rio)


dir_data = "raw_data"
dir_lookups = "lookups_data"


pdf1 = file.path(dir_data, "pdf1.xlsx")
pdf2 = file.path(dir_data, "pdf2.xlsx")


##crear nombre de columnas -----------------------------------------------------
test = read_xlsx(pdf2, sheet = "Table 373") 

names <- as.character(test[1,])
#fuera = which(names %in% c(NA,'NA'))
#names = names[-fuera]


#read lookups 
look_ups = import(file.path(dir_lookups, "lookup.rds"))
provincias = look_ups$provincia
comunidades = "CATALUÑA"
provincias

#importar functions

lapply(list.files("functions"), function(x){source(file.path("functions",x))})


