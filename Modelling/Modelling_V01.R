####
# | Criado em: 2020-05-10 |
# PROCESSOS TRANSFORMACAO URBBANA
# MODELACAO Versão 01 | 2020-05-10   ----
####

####
# 0. LIBRARIES ----
####

library("tidyverse")
library("sf")
library("tidyselec")



####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA


setwd(paste(path, "02_RECURSOS/023_DSS_COMPONENTES/024_DEMOG_ECON/0241_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/Modelling", sep=""))


OUT.tables.path = paste(path, "02_RECURSOS/023_DSS_COMPONENTES/024_DEMOG_ECON/0241_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")
