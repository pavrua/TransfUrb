####
# | Criado em: 2020-06-15 |
# PROCESSOS TRANSFORMACAO URBBANA
# MODELACAO «TRANSFORMACOES URBANANAS - INTERACOES ESPACIAIS» Versao 01 | 2020-06-15   ----
####

####
# 0. LIBRARIES ----
####

library("tidyverse")


library("xlsx")
library("openxlsx")

library("psych")
library("nFactors")

library("ggplot2")

library("spdep")
library("gstat")

library("sf")
library("rgdal")
library("maptools")

library("RColorBrewer")
library("classInt")

library("stringr")
#library("magrittr")



####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/Modelling", sep=""))

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")
sourceSIG_GEODATA_VARIOS_gdb = paste(path, "01_BASESDEDADOS/SIG/GEODATA_VARIOS.gdb", sep = "")

pathDADOSBASE_DRIVITUP_POPPROJECCOES =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GE5anos_PROJECCOES/", sep ="") 
pathDADOSBASE_DRIVITUP_INE_ESTIMATIVAS =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GE5anos__INE_ESTIMATIVAS/", sep ="") 
pathDADOSBASE_DRIVITUP_POPCENSOS =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GE5anos_CENSOS_91_01_11/", sep ="") 
pathDADOSBASE_DRIVITUP_GEODESCODIFICA =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/CAOP_DESCODIFICA/", sep ="") 
pathDADOSBASE_DRIVITUP_BGRI2011 =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/CENSOS2011_BGRI/", sep ="") 
pathDADOSBASE_DRIVITUP_INEPEDIDO1 = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/INE_PEDIDO_1/", sep ="") 
pathDADOSBASE_DRIVITUP_INEPEDIDO2 = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/INE_PEDIDO_2/", sep ="") 
pathDADOSBASE_DRIVITUP_TABELASVARIAS = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/INE_TABELAS_VARIAS/", sep ="") 
pathDADOSBASE_DRIVITUP_POPGLOBAL = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GLOBAL/", sep ="") 



OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")
OUT.RData.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.RData/", sep = "")

#ArcGIS_BRIDGE = paste(path, "01_BASESDEDADOS/SIG/Z_ArcGIS_BRIDGE", sep = "")


####
# 0.1 LOAD PREPROCESSED DATA ----
####


#CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")
#CAOP2018_FREG_Descodifica = CAOP2018_FREG_Descodifica.shp %>% st_drop_geometry()

#DADOS DEMOGRAFICOS (para var POP)
#FICHEIROS: BD_DEMOALL, OUT.POPCENSOS_2001e2011, OUT.POTDEMO2011byFREG2018, OUT.POP_PPF
load( paste(OUT.RData.path,"DEMOGRAFIA_01_11_15_CAOP2018.RData", sep="") )

#DADOS LOCATION 
#FICHEIROS: Acess_BD_withAFeCA_FINAL, OUT.CA_AF_FINAL_groups
load( paste(OUT.RData.path,"FREG_CAOP2018_Location_data.RData", sep="") )

#DADOS LOCALITY 
#FICHEIROS: db.COS_COMBINE, OUT.CA_COS_COMBINE_F0_Wardgroups
load( paste(OUT.RData.path,"FREG_CAOP2018_Locality_data.RData", sep="") )

#DADOS TU CLASSIFICATION V8 
#FICHEIROS: db.COS_COMBINE, OUT.CA_COS_COMBINE_F0_Wardgroups
load( paste(OUT.RData.path,"FREG_CAOP2018_CLASSIFICATION_V8.RData", sep="") )



####
# 1.Testes ----
####





####
# 2. SPATIAL ANALYSIS  ####
####

# 2.0.1 GET AND PREPARE SPATIAL DATA ####

#muni_pt <- readOGR(dsn = pathDADOSBASE_SIG , layer = "CAOP2018_MUNI", encoding = "UTF-8", use_iconv = TRUE)

#Read shapefile of Municipalities in PT - using library sf
shape_freg18_pt <- st_read(dsn = pathDADOSBASE_SIG , layer = "CAOP2018_MUNI")

shape_muni_pt.alt <- st_read(dsn = pathDADOSBASE_SIG , layer = "CAOP2018_MUNI_simplifiedForSpatAnaly3")



#Add data to polygons
shape_muni_pt.addData = merge(shape_muni_pt, AFFORDABILITY, by.x = "MUNICOD18", by.y = "COD_MUNI")
shape_muni_pt.addData = merge(shape_muni_pt.addData, OUT.ALOJDISPON_POP, by.x = "MUNICOD18", by.y = "MUNICIPIO")

shape_muni_pt.addData$HousePriceToIncomeFinal = shape_muni_pt.addData$HousePriceToIncome
shape_muni_pt.addData$HousePriceToIncomeFinal[is.na(shape_muni_pt.addData$HousePriceToIncomeFinal)] <- mean(shape_muni_pt.addData$HousePriceToIncomeFinal, na.rm = T)


# create row-standardized Queens contiguity weights matrix

# Queen 1st order
muni_pt.nb_q_1order <- poly2nb(shape_muni_pt.addData)  #queen's neighborhood
muni_pt.nb_q_1order.w <- nb2listw(muni_pt.nb_q_1order)





####
# 0 SAVES  ----
####

# write_delim(BD_TU_GLOBAL, paste(OUT.tables.path,"FREG_CAOP2018_TRANSFURB_GLOBAL_data.txt", sep=""), quote_escape = "backslash")
# 
# save(BD_TU_GLOBAL, OUT.TRANSF_URB,
#      file = paste(OUT.RData.path, "FREG_CAOP2018_TRANSFURB_GLOBAL_data.RData", sep="") )
# 




