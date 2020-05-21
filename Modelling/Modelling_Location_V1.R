####
# | Criado em: 2020-05-10 |
# PROCESSOS TRANSFORMACAO URBBANA
# MODELACAO Versao 01 | 2020-05-10   ----
####

####
# 0. LIBRARIES ----
####

library("tidyverse")
library("sf")
#library("tidyselec")

library("psych")
library("nFactors")


####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/Modelling", sep=""))

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")

OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")
OUT.RData.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.RData/", sep = "")



####
# 0.1 LOAD PREPROCESSED DATA ----
####


CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")
CAOP2018_FREG_Descodifica = CAOP2018_FREG_Descodifica.shp %>% st_drop_geometry()

#DADOS DEMOGRAFICOS (para potencial demografico)
#FICHEIROS: BD_DEMOALL, OUT.POPCENSOS_2001e2011, OUT.POTDEMO2011byFREG2018, OUT.POP_PPF
load( paste(OUT.RData.path,"DEMOGRAFIA_01_11_15_CAOP2018.RData", sep="") )

#DADOS POIs (para potencial de acessibilidades)
#FICHEIROS: all.dados.sig.withoutNA.CAOP2018.unique.F, amenities.list.all.getData, 
# shops.list.all.getData, leisure.list.all.getData, tourism.list.all.getData
# amenities.all.dados.sig.withoutNA.CAOP2018, shops.all.dados.sig.withoutNA.CAOP2018,
# leisure.all.dados.sig.withoutNA.CAOP2018, tourism.all.dados.sig.withoutNA.CAOP2018
load( paste(OUT.RData.path,"POIsProcessing.RData", sep="") )



####
# 0.2 PREPROCESSING DISTANCE MATRIX  ----
####

CAOP2018_FREG_Descodifica$ID = seq(1,nrow(CAOP2018_FREG_Descodifica))
CAOP2018_FREG_Descodifica$raio = sqrt(CAOP2018_FREG_Descodifica$AREAFREG18 / pi)



FREG18_DistMatrix = read_sf(dsn = sourceSIG_GEODATA_VARIOS_gdb, layer = "CAOP2018_FREG_ONEPOLY_pointCentroid_DISTMATRIX")


FREG18_DistMatrixLabels = merge(FREG18_DistMatrix, CAOP2018_FREG_Descodifica[,c("ID","DICOFRE18")], by.x ="INPUT_FID" , by.y = "ID", all.x = T)
colnames(FREG18_DistMatrixLabels)[4] = "INPUT_DICOFRE18"
FREG18_DistMatrixLabels = merge(FREG18_DistMatrixLabels, CAOP2018_FREG_Descodifica[,c("ID","DICOFRE18")], by.x = "NEAR_FID" , by.y = "ID", all.x = T)
colnames(FREG18_DistMatrixLabels)[5] = "NEAR_DICOFRE18"

FREG18_DistMatrixLabels_Wider = FREG18_DistMatrixLabels[, c("DISTANCE", "INPUT_DICOFRE18", "NEAR_DICOFRE18")] %>%
  pivot_wider(names_from = NEAR_DICOFRE18, values_from = DISTANCE)

FREG18_DistMatrixLabels_WiderF = FREG18_DistMatrixLabels_Wider[order(FREG18_DistMatrixLabels_Wider$INPUT_DICOFRE18), ]


FREG18_DistMatrixLabels_WiderF_Matrix = as.matrix( select(FREG18_DistMatrixLabels_WiderF, -c("INPUT_DICOFRE18")) )
diag(FREG18_DistMatrixLabels_WiderF_Matrix) = CAOP2018_FREG_Descodifica$raio


####
# 1 ACESSIBILITY  ----
####

foo <- mapvalues(foo, from=c("AA", "AC", "AG"), to=c("0101", "0102", "0103"))


