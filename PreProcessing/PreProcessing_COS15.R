####
# | 2020-05-11 |
# PROCESSOS TRANSFORMACAO URBBANA
# GET DATA AND PRE-PROCESSING  ----
####

####
# 0. LIBRARIES ----
####

library("tidyverse")
library("sf")
#library("tidyselec")

library("reshape2") 
#reshape2 :: usa as funções dcast e melt equivalents para fazer pivot tables
# no tidyverse temos o  tidyr com as funcoes pivot_



####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/PreProcessing", sep=""))


OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")
OUT.RData.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.RData/", sep = "")


sourceTABLE_CAOP12v1CAOP18_DESCODIF = paste(path, "01_BASESDEDADOS/TABELAS/CAOP_DESCODIFICA/", sep="")


sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")
sourceSIG_GEODATA_VARIOS_gdb = paste(path, "01_BASESDEDADOS/SIG/GEODATA_VARIOS.gdb", sep = "")
sourceSIG_COS_gdb = paste(path, "01_BASESDEDADOS/SIG/COS/COS.gdb", sep = "")







####
# 2. COS ----
####

# * 3.1 COS2015 ----

COS2015withCAOP2012v1eCAOP2018.shp = read_sf(dsn = sourceSIG_COS_gdb, layer = "COS2015withCAOP2012v1eCAOP2018")
COS2015withCAOP2012v1eCAOP2018 = COS2015withCAOP2012v1eCAOP2018.shp %>% st_drop_geometry()

# Define all categories
COS2015withCAOP2012v1eCAOP2018$COS2015CAT = COS2015withCAOP2012v1eCAOP2018$COS2015_V1
COS2015withCAOP2018F = COS2015withCAOP2012v1eCAOP2018 %>% separate(COS2015_V1, c("COS15_Nivel1", "COS15_Nivel2", "COS15_Nivel3", "COS15_Nivel4"), remove = F)
COS2015withCAOP2018F = unite(COS2015withCAOP2018F, col = "COS15_Nivel12", c("COS15_Nivel1", "COS15_Nivel2"), remove = F)
COS2015withCAOP2018F = unite(COS2015withCAOP2018F, col = "COS15_Nivel123", c("COS15_Nivel1", "COS15_Nivel2", "COS15_Nivel3"), remove = F)


# Lista de categorias

OUT.COS2015_categorias = COS2015withCAOP2018F %>% 
  group_by( COS2015_V1, COS2015_Le, Megaclasse, COS15_Nivel123, COS15_Nivel12 ) %>% 
  summarise(
    
    AREAT = sum(COS15_AREAPART)
  )
OUT.COS2015_categorias[OUT.COS2015_categorias==""] = NA
OUT.COS2015_categorias = OUT.COS2015_categorias[complete.cases(OUT.COS2015_categorias$COS2015_V1),]




# OUTPUT byCAOP2018 E CATEGORIAS NIVEL 1

OUT.COS2015withCAOP2018F_N1_T0 = COS2015withCAOP2018F %>% 
  group_by( DICOFRE18, FREG18_la, COS15_Nivel1 ) %>% 
  summarise(
    AREAFREG18_N1 = mean(AREAFREG18, rm.na = T),
    AREACateg_N1 = sum(COS15_AREAPART, rm.na = T),
    
  )
OUT.COS2015withCAOP2018F_N1_T0[OUT.COS2015withCAOP2018F_N1_T0==""] = NA
OUT.COS2015withCAOP2018F_N1_T0 = OUT.COS2015withCAOP2018F_N1_T0[complete.cases(OUT.COS2015withCAOP2018F_N1_T0$DICOFRE18),]
OUT.COS2015withCAOP2018F_N1_T0 = OUT.COS2015withCAOP2018F_N1_T0[complete.cases(OUT.COS2015withCAOP2018F_N1_T0$COS15_Nivel1),]
OUT.COS2015withCAOP2018F_N1_T0.wide = 
  OUT.COS2015withCAOP2018F_N1_T0 %>%  pivot_wider("COS15_Nivel1", "AREACateg_N1")


# OUTPUT byCAOP2018, CATEGORIA 1 e NIVEL 1e2

OUT.COS2015withCAOP2018F_N12_T1 = filter(COS2015withCAOP2018F, COS15_Nivel1 == 1)  %>% 
  group_by( DICOFRE18, FREG18_la, COS15_Nivel12 ) %>% 
  summarise(
    AREAFREG18_N12_T1 = mean(AREAFREG18, rm.na = T),
    AREACateg_N12_T1 = sum(COS15_AREAPART, rm.na = T),
    
  )
OUT.COS2015withCAOP2018F_N12_T1[OUT.COS2015withCAOP2018F_N12_T1==""] = NA
OUT.COS2015withCAOP2018F_N12_T1 = OUT.COS2015withCAOP2018F_N12_T1[complete.cases(OUT.COS2015withCAOP2018F_N12_T1$DICOFRE18),]
OUT.COS2015withCAOP2018F_N12_T1 = OUT.COS2015withCAOP2018F_N12_T1[complete.cases(OUT.COS2015withCAOP2018F_N12_T1$COS15_Nivel12),]
OUT.COS2015withCAOP2018F_N12_T1.wide = 
  OUT.COS2015withCAOP2018F_N12_T1 %>%  pivot_wider("COS15_Nivel12", "AREACateg_N12_T1")


# OUTPUT byCAOP2018, CATEGORIA 1 e NIVEL 1e2e3

OUT.COS2015withCAOP2018F_N123_T1 = filter(COS2015withCAOP2018F, COS15_Nivel1 == 1)  %>% 
  group_by( DICOFRE18, FREG18_la, COS15_Nivel123 ) %>% 
  summarise(
    AREAFREG18_N123_T1 = mean(AREAFREG18, rm.na = T),
    AREACateg_N123_T1 = sum(COS15_AREAPART, rm.na = T),
    
  )
OUT.COS2015withCAOP2018F_N123_T1[OUT.COS2015withCAOP2018F_N123_T1==""] = NA
OUT.COS2015withCAOP2018F_N123_T1 = OUT.COS2015withCAOP2018F_N123_T1[complete.cases(OUT.COS2015withCAOP2018F_N123_T1$DICOFRE18),]
OUT.COS2015withCAOP2018F_N123_T1 = OUT.COS2015withCAOP2018F_N123_T1[complete.cases(OUT.COS2015withCAOP2018F_N123_T1$COS15_Nivel123),]
OUT.COS2015withCAOP2018F_N123_T1.wide = 
  OUT.COS2015withCAOP2018F_N123_T1 %>%  pivot_wider("COS15_Nivel123", "AREACateg_N123_T1")



####
# 0. SAVE GERAL ----
####

COS2015withCAOP2018_BDALL = left_join(OUT.COS2015withCAOP2018F_N1_T0.wide, OUT.COS2015withCAOP2018F_N12_T1.wide, by = c("DICOFRE18" = "DICOFRE18"))
COS2015withCAOP2018_BDALL = left_join(COS2015withCAOP2018_BDALL, OUT.COS2015withCAOP2018F_N123_T1.wide, by = c("DICOFRE18" = "DICOFRE18"))

#COS2015withCAOP2018_BDALL[is.na(COS2015withCAOP2018_BDALL)] = 0 


save(COS2015withCAOP2018_BDALL,
     OUT.COS2015withCAOP2018F_N1_T0.wide,
     OUT.COS2015withCAOP2018F_N12_T1.wide,
     OUT.COS2015withCAOP2018F_N123_T1.wide,
     OUT.COS2015_categorias,
     file = paste(OUT.RData.path, "COS2015N123T1234T1byCAOP2018.RData", sep="") )


