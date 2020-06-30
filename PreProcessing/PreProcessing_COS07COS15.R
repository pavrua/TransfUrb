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

# * 2.1 COS2015 ----

COS2015withCAOP2012v1eCAOP2018.shp = read_sf(dsn = sourceSIG_COS_gdb, layer = "COS2015withCAOP2012v1eCAOP2018")
COS2015withCAOP2012v1eCAOP2018 = COS2015withCAOP2012v1eCAOP2018.shp %>% st_drop_geometry()

# Define all categories
COS2015withCAOP2012v1eCAOP2018$COS2015CAT = COS2015withCAOP2012v1eCAOP2018$COS2015_V1
COS2015withCAOP2018F = COS2015withCAOP2012v1eCAOP2018 %>% separate(COS2015_V1, c("COS15_Nivel1", "COS15_Nivel2", "COS15_Nivel3", "COS15_Nivel4"), remove = F)
COS2015withCAOP2018F = unite(COS2015withCAOP2018F, col = "COS15_Nivel12", c("COS15_Nivel1", "COS15_Nivel2"), remove = F)
COS2015withCAOP2018F = unite(COS2015withCAOP2018F, col = "COS15_Nivel123", c("COS15_Nivel1", "COS15_Nivel2", "COS15_Nivel3"), remove = F)


# Lista de categorias

OUT.COS2015_N3 = COS2015withCAOP2018F %>% 
  group_by( COS15_Nivel123, COS15_Nivel12, COS2015_V1, COS2015_Le, Megaclasse ) %>% 
  summarise(
    
    COS15_N3_AREAT = sum(COS15_AREAPART)
  )
OUT.COS2015_N3[OUT.COS2015_N3==""] = NA
OUT.COS2015_N3 = OUT.COS2015_N3[complete.cases(OUT.COS2015_N3$COS2015_V1),]




# OUTPUT byCAOP2018 E CATEGORIAS NIVEL 1

OUT.COS2015_N1_T0_FREG18 = COS2015withCAOP2018F %>% 
  group_by( DICOFRE18, FREG18_la, COS15_Nivel1 ) %>% 
  summarise(
    AREAFREG18_N1_T0 = mean(AREAFREG18, rm.na = T),
    AREA_N1_byFREG18 = sum(COS15_AREAPART, rm.na = T),
    
  )
OUT.COS2015_N1_T0_FREG18[OUT.COS2015_N1_T0_FREG18==""] = NA
OUT.COS2015_N1_T0_FREG18 = OUT.COS2015_N1_T0_FREG18[complete.cases(OUT.COS2015_N1_T0_FREG18$DICOFRE18),]
OUT.COS2015_N1_T0_FREG18 = OUT.COS2015_N1_T0_FREG18[complete.cases(OUT.COS2015_N1_T0_FREG18$COS15_Nivel1),]
OUT.COS2015_N1_T0_FREG18.wide = pivot_wider(OUT.COS2015_N1_T0_FREG18,  names_from = "COS15_Nivel1", values_from = "AREA_N1_byFREG18")


# OUTPUT byCAOP2018, CATEGORIA 1 e NIVEL 1e2

OUT.COS2015_N12_T1_FREG18 = filter(COS2015withCAOP2018F, COS15_Nivel1 == 1)  %>% 
  group_by( DICOFRE18, FREG18_la, COS15_Nivel12 ) %>% 
  summarise(
    AREAFREG18_N12_T1 = mean(AREAFREG18, rm.na = T),
    AREA_N12_T1_byFREG18 = sum(COS15_AREAPART, rm.na = T),
    
  )
OUT.COS2015_N12_T1_FREG18[OUT.COS2015_N12_T1_FREG18==""] = NA
OUT.COS2015_N12_T1_FREG18 = OUT.COS2015_N12_T1_FREG18[complete.cases(OUT.COS2015_N12_T1_FREG18$DICOFRE18),]
OUT.COS2015_N12_T1_FREG18 = OUT.COS2015_N12_T1_FREG18[complete.cases(OUT.COS2015_N12_T1_FREG18$COS15_Nivel12),]
OUT.COS2015_N12_T1_FREG18.wide = 
  OUT.COS2015_N12_T1_FREG18 %>%  pivot_wider(names_from = "COS15_Nivel12", values_from = "AREA_N12_T1_byFREG18")


# OUTPUT byCAOP2018, CATEGORIA 1 e NIVEL 1e2e3

OUT.COS2015_N123_T1_FREG18 = filter(COS2015withCAOP2018F, COS15_Nivel1 == 1)  %>% 
  group_by( DICOFRE18, FREG18_la, COS15_Nivel123 ) %>% 
  summarise(
    AREAFREG18_N123_T1 = mean(AREAFREG18, rm.na = T),
    AREA_N123_T1_byFREG18 = sum(COS15_AREAPART, rm.na = T),
    
  )
OUT.COS2015_N123_T1_FREG18[OUT.COS2015_N123_T1_FREG18==""] = NA
OUT.COS2015_N123_T1_FREG18 = OUT.COS2015_N123_T1_FREG18[complete.cases(OUT.COS2015_N123_T1_FREG18$DICOFRE18),]
OUT.COS2015_N123_T1_FREG18 = OUT.COS2015_N123_T1_FREG18[complete.cases(OUT.COS2015_N123_T1_FREG18$COS15_Nivel123),]
OUT.COS2015_N123_T1_FREG18.wide = 
  OUT.COS2015_N123_T1_FREG18 %>%  pivot_wider(names_from = "COS15_Nivel123", values_from = "AREA_N123_T1_byFREG18")



#-------------------#
# * 2.2 COS2007 ----

COS2007withCAOP2018.shp = read_sf(dsn = sourceSIG_COS_gdb, layer = "COS2007withCAOP2018")
COS2007withCAOP2018 = COS2007withCAOP2018.shp %>% st_drop_geometry()

# Define all categories
COS2007withCAOP2018$COS2007CAT = COS2007withCAOP2018$COS_Cod
COS2007withCAOP2018F = COS2007withCAOP2018 %>% separate(COS_Cod, c("COS07_Nivel1", "COS07_Nivel2", "COS07_Nivel3", "COS07_Nivel4"), remove = F)
COS2007withCAOP2018F = unite(COS2007withCAOP2018F, col = "COS07_Nivel12", c("COS07_Nivel1", "COS07_Nivel2"), remove = F)
COS2007withCAOP2018F = unite(COS2007withCAOP2018F, col = "COS07_Nivel123", c("COS07_Nivel1", "COS07_Nivel2", "COS07_Nivel3"), remove = F)


# Lista de categorias

OUT.COS2007_N3 = COS2007withCAOP2018F %>% 
  group_by( COS07_Nivel123, COS07_Nivel12 ) %>% 
  summarise(
    COS_Cod_simplified = first(COS_Cod),
    descricao_simplified = first(descricao),
    COS07_N3_AREAT = sum(COS07_AREAPART)
  )
OUT.COS2007_N3[OUT.COS2007_N3==""] = NA
#OUT.COS2007_N3 = OUT.COS2007_N3[complete.cases(OUT.COS2007_N3$COS_Cod),]




# OUTPUT byCAOP2018 E CATEGORIAS NIVEL 1

OUT.COS2007_N1_T0_FREG18 = COS2007withCAOP2018F %>% 
  group_by( DICOFRE18, FREG18_la, COS07_Nivel1 ) %>% 
  summarise(
    AREAFREG18_N1_T0 = mean(AFREG18, rm.na = T),
    AREA_N1_byFREG18 = sum(COS07_AREAPART, rm.na = T),
    
  )
OUT.COS2007_N1_T0_FREG18[OUT.COS2007_N1_T0_FREG18==""] = NA
OUT.COS2007_N1_T0_FREG18 = OUT.COS2007_N1_T0_FREG18[complete.cases(OUT.COS2007_N1_T0_FREG18$DICOFRE18),]
OUT.COS2007_N1_T0_FREG18 = OUT.COS2007_N1_T0_FREG18[complete.cases(OUT.COS2007_N1_T0_FREG18$COS07_Nivel1),]
OUT.COS2007_N1_T0_FREG18.wide = pivot_wider(OUT.COS2007_N1_T0_FREG18,  names_from = "COS07_Nivel1", values_from = "AREA_N1_byFREG18")


# OUTPUT byCAOP2018, CATEGORIA 1 e NIVEL 1e2

OUT.COS2007_N12_T1_FREG18 = filter(COS2007withCAOP2018F, COS07_Nivel1 == 1)  %>% 
  group_by( DICOFRE18, FREG18_la, COS07_Nivel12 ) %>% 
  summarise(
    AREAFREG18_N12_T1 = mean(AFREG18, rm.na = T),
    AREA_N12_T1_byFREG18 = sum(COS07_AREAPART, rm.na = T),
    
  )
OUT.COS2007_N12_T1_FREG18[OUT.COS2007_N12_T1_FREG18==""] = NA
OUT.COS2007_N12_T1_FREG18 = OUT.COS2007_N12_T1_FREG18[complete.cases(OUT.COS2007_N12_T1_FREG18$DICOFRE18),]
OUT.COS2007_N12_T1_FREG18 = OUT.COS2007_N12_T1_FREG18[complete.cases(OUT.COS2007_N12_T1_FREG18$COS07_Nivel12),]
OUT.COS2007_N12_T1_FREG18.wide = 
  OUT.COS2007_N12_T1_FREG18 %>%  pivot_wider(names_from = "COS07_Nivel12", values_from = "AREA_N12_T1_byFREG18")


# OUTPUT byCAOP2018, CATEGORIA 1 e NIVEL 1e2e3

OUT.COS2007_N123_T1_FREG18 = filter(COS2007withCAOP2018F, COS07_Nivel1 == 1)  %>% 
  group_by( DICOFRE18, FREG18_la, COS07_Nivel123 ) %>% 
  summarise(
    AREAFREG18_N123_T1 = mean(AFREG18, rm.na = T),
    AREA_N123_T1_byFREG18 = sum(COS07_AREAPART, rm.na = T),
    
  )
OUT.COS2007_N123_T1_FREG18[OUT.COS2007_N123_T1_FREG18==""] = NA
OUT.COS2007_N123_T1_FREG18 = OUT.COS2007_N123_T1_FREG18[complete.cases(OUT.COS2007_N123_T1_FREG18$DICOFRE18),]
OUT.COS2007_N123_T1_FREG18 = OUT.COS2007_N123_T1_FREG18[complete.cases(OUT.COS2007_N123_T1_FREG18$COS07_Nivel123),]
OUT.COS2007_N123_T1_FREG18.wide = 
  OUT.COS2007_N123_T1_FREG18 %>%  pivot_wider(names_from = "COS07_Nivel123", values_from = "AREA_N123_T1_byFREG18")





####
# 3. COMBINE COS07&COS15 ----
####

COS2015_BDALL_byFRE18 = left_join(OUT.COS2015_N1_T0_FREG18.wide, OUT.COS2015_N12_T1_FREG18.wide, by = c("DICOFRE18" = "DICOFRE18"))
COS2015_BDALL_byFRE18 = left_join(COS2015_BDALL_byFRE18, OUT.COS2015_N123_T1_FREG18.wide, by = c("DICOFRE18" = "DICOFRE18"))


COS2007_BDALL_byFRE18 = left_join(OUT.COS2007_N1_T0_FREG18.wide, OUT.COS2007_N12_T1_FREG18.wide, by = c("DICOFRE18" = "DICOFRE18"))
COS2007_BDALL_byFRE18 = left_join(COS2007_BDALL_byFRE18, OUT.COS2007_N123_T1_FREG18.wide, by = c("DICOFRE18" = "DICOFRE18"))


COS_A07A15_BDALL_byFRE18 = left_join(COS2007_BDALL_byFRE18, COS2015_BDALL_byFRE18, by = c("DICOFRE18" = "DICOFRE18"))

colreplace = colnames(COS_A07A15_BDALL_byFRE18)

colreplace = str_replace(colreplace, ".y", "_COS15")
colreplace = str_replace(colreplace, ".y", "")

colreplace = str_replace(colreplace, ".x", "_COS07")
colreplace = str_replace(colreplace, ".x", "")

colreplace = str_replace(colreplace, "^[1-9]", paste("V_",str_sub(colreplace, 1, 1),sep="") )
colreplace = str_replace(colreplace, ".x", "L")
colreplace = str_replace(colreplace, ".y", "L")


colnames(COS_A07A15_BDALL_byFRE18) = colreplace



####
# 3. VARIACOES ----
####

COS_A07A15_BDALL_byFRE18[is.na(COS_A07A15_BDALL_byFRE18)] = 0

OUT.VAR_COS07_COS15_FREG18 =  COS_A07A15_BDALL_byFRE18 %>% 
  group_by( DICOFRE18 ) %>% 
  summarise(
    
    AREAFREG18 = mean(AREAFREG18_N1_T0_COS15),
    
    V_Prop_1_COS07 = V_1_COS07 / AREAFREG18,
    V_Prop_2_COS07  = V_2_COS07 / AREAFREG18,
    V_Prop_3_COS07  =  V_3_COS07 / AREAFREG18 ,
    V_Prop_11_COS07  = V_1_1_COS07 / AREAFREG18,
    V_Prop_12_COS07  = V_1_2_COS07 / AREAFREG18,
    V_Prop_13_COS07  = V_1_3_COS07 / AREAFREG18,
    V_Prop_14_COS07  = V_1_4_COS07 / AREAFREG18,
    V_Prop_111_COS07  = V_1_1_1_COS07 / AREAFREG18,
    V_Prop_112_COS07  = V_1_1_2_COS07 / AREAFREG18,
    
    V_Prop_1_COS15 = V_1_COS15 / AREAFREG18,
    V_Prop_2_COS15  = V_2_COS15 / AREAFREG18,
    V_Prop_3_COS15  = V_3_COS15 / AREAFREG18,
    V_Prop_11_COS15  = V_1_1_COS15 / AREAFREG18,
    V_Prop_12_COS15  = V_1_2_COS15 / AREAFREG18,
    V_Prop_13_COS15  = V_1_3_COS15 / AREAFREG18,
    V_Prop_14_COS15  = V_1_4_COS15 / AREAFREG18,
    V_Prop_111_COS15  = V_1_1_1_COS15 / AREAFREG18,
    V_Prop_112_COS15  = V_1_1_2_COS15 / AREAFREG18,
    
    V_1_ChangeProp0715 = round( (V_Prop_1_COS15 - V_Prop_1_COS07) / V_Prop_1_COS07 ,2),
    V_2_ChangeProp0715 = round( (V_Prop_2_COS15 - V_Prop_2_COS07) / V_Prop_2_COS07 ,2),
    V_3_ChangeProp0715 = round( (V_Prop_3_COS15 - V_Prop_3_COS07) / V_Prop_3_COS07 ,2),
    V_11_ChangeProp0715 = round( (V_Prop_11_COS15 - V_Prop_11_COS07) / V_Prop_11_COS07 ,2),
    V_12_ChangeProp0715 = round( (V_Prop_12_COS15 - V_Prop_12_COS07) / V_Prop_12_COS07,2),
    V_13_ChangeProp0715 = round( (V_Prop_13_COS15 - V_Prop_13_COS07) / V_Prop_13_COS07,2),
    V_14_ChangeProp0715 = round( (V_Prop_14_COS15 - V_Prop_14_COS07) / V_Prop_14_COS07,2),
    V_111_ChangeProp0715 = round( (V_Prop_111_COS15 - V_Prop_111_COS07) / V_Prop_111_COS07,2),
    V_112_ChangeProp0715 = round( (V_Prop_112_COS15 - V_Prop_112_COS07) / V_Prop_112_COS07,2),
    
    
    V_1_ChangeArea0715 = round( (V_1_COS15 - V_1_COS07) / V_1_COS07 ,2),
    V_2_ChangeArea0715 = round( (V_2_COS15 - V_2_COS07) / V_2_COS07 ,2),
    V_3_ChangeArea0715 = round( (V_3_COS15 - V_3_COS07) / V_3_COS07 ,2),
    V_11_ChangeArea0715 = round( (V_1_1_COS15 - V_1_1_COS07) / V_1_1_COS07 ,2),
    V_12_ChangeArea0715 = round( (V_1_2_COS15 - V_1_2_COS07) / V_1_2_COS07,2),
    V_13_ChangeArea0715 = round( (V_1_3_COS15 - V_1_3_COS07) / V_1_3_COS07,2),
    V_14_ChangeArea0715 = round( (V_1_4_COS15 - V_1_4_COS07) / V_1_4_COS07,2),
    V_111_ChangeArea0715 = round( (V_1_1_1_COS15 - V_1_1_1_COS07) / V_1_1_1_COS07,2),
    V_112_ChangeArea0715 = round( (V_1_1_2_COS15 - V_1_1_2_COS07) / V_1_1_2_COS07,2)
    
    
    
    
  )








COS_FINAL_A07A15_byFRE18 = left_join(COS_A07A15_BDALL_byFRE18, OUT.VAR_COS07_COS15_FREG18, by = c("DICOFRE18" = "DICOFRE18"))



COS_FINALSEL_A07A15_byFRE18 = COS_FINAL_A07A15_byFRE18[, c("DICOFRE18", "AREAFREG18", "FREG18_la_COS07",
                                      "V_1_1_COS07", "V_1_1_COS15", 
                                      "V_11_ChangeArea0715",
                                      "V_Prop_11_COS07", "V_Prop_11_COS15", 
                                      "V_11_ChangeProp0715",
                                      "V_Prop_111_COS15", "V_Prop_112_COS15",
                                      "V_Prop_111_COS07", "V_Prop_112_COS07",
                                      "V_111_ChangeProp0715", "V_112_ChangeProp0715"
)]


COS_FINALSEL_A07A15_byFRE18[is.na(COS_FINALSEL_A07A15_byFRE18)] = 0


####
# 0. SAVE GERAL ----
####

save(COS2015_BDALL_byFRE18,
     OUT.COS2015_N3,
     OUT.COS2015_N1_T0_FREG18.wide,
     OUT.COS2015_N12_T1_FREG18.wide,
     OUT.COS2015_N123_T1_FREG18.wide,
     
     COS2007_BDALL_byFRE18,
     OUT.COS2007_N3,
     OUT.COS2007_N1_T0_FREG18.wide,
     OUT.COS2007_N12_T1_FREG18.wide,
     OUT.COS2007_N123_T1_FREG18.wide,
     
     COS_FINAL_A07A15_byFRE18,
     
     COS_FINALSEL_A07A15_byFRE18,
     
     file = paste(OUT.RData.path, "COS2007eCOS2015.RData", sep="") )


