####
# | Criado em: 2020-05-28 |
# PROCESSOS TRANSFORMACAO URBBANA
# MODELACAO «TRANSFORMACOES URBANANAS FINAL» Versao 01 | 2020-05-28   ----
####

####
# 0. LIBRARIES ----
####

library("tidyverse")
library("sf")
#library("tidyselec")
#library("dplyr")


library("xlsx")

library("psych")
library("nFactors")

library("BAMMtools")


####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/Modelling", sep=""))

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")
sourceSIG_GEODATA_VARIOS_gdb = paste(path, "01_BASESDEDADOS/SIG/GEODATA_VARIOS.gdb", sep = "")

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



db.ALL = left_join(db.ACESSIBILITY,OUT.POPCENSOS_2001e2011, by = c("DICOFRE18", "DICOFRE18") )
db.ALL = left_join(db.ALL, db.COS_COMBINE, by = c("DICOFRE18", "DICOFRE18") )

db.ALL$CL_LOCALITY = db.ALL$CA_COS_COMBINE_F0_Wardgroups
db.ALL$CL_LOCATION = db.ALL$fit_CA_AF_FINAL_groups

db.ALL$FA_LOCALITY = db.ALL$FA_LOCALITY_COMBINED_scale
db.ALL$FA_LOCATION = db.ALL$FA_FINAL_Fscores

db.ALL$POPCHANGE_scale = scale(db.ALL$POP_VarPop_C2001C2011)

####
# 1.Testes ----
####

# 1.1 Clusters from factors and pop  ----

CA_ORIF_Ward7 = db.ALL[,c(
  "FA_FINAL_Fscores", "FA_COSCOMBINE_Fscores", "POPCHANGE_scale")
  ]


# Ward Hierarchical Clustering
d.CA_ORIF_Ward7  <- dist(CA_ORIF_Ward7, method = "euclidean") # distance matrix
fit.CA_ORIF_Ward7 <- hclust(d.CA_ORIF_Ward7, method="ward.D")
plot(fit.CA_ORIF_Ward7) # display dendogram
CA_ORIF_Ward7_groups <- cutree(fit.CA_ORIF_Ward7, k=7) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_ORIF_Ward7, k=7, border="red")


db.ALL = cbind(db.ALL, CA_ORIF_Ward7_groups)

OUT.CA_ORIF_Ward7  = db.ALL %>% 
  group_by( CA_ORIF_Ward7_groups ) %>% 
  summarise(
    N_Cases = n(),
    Locality_F_mean =  round(mean(FA_LOCALITY, na.rm = TRUE),2),
    Location_F_mean =  round(mean(FA_LOCATION, na.rm = TRUE),2),
    VarPop_mean =  round(mean(POP_VarPop_C2001C2011, na.rm = TRUE),2),
  )

# 1.2 Outros  ----

# Quintiles 
db.ALL$LOCATION_quintile = cut(db.ALL$FA_FINAL_Fscores, breaks = quantile(db.ALL$FA_FINAL_Fscores, seq(0, 1, .2)), labels = c(1,2,3,4,5), include.lowest = TRUE )
db.ALL$LOCALITY_quintile = cut(db.ALL$FA_COSCOMBINE_Fscores, breaks = quantile(db.ALL$FA_COSCOMBINE_Fscores, seq(0, 1, .2)), labels = c(1,2,3,4,5), include.lowest = TRUE )
db.ALL$POPCHANGE_quintile = cut(db.ALL$POP_VarPop_C2001C2011, breaks = quantile(db.ALL$POP_VarPop_C2001C2011, seq(0, 1, .2)), labels = c(1,2,3,4,5), include.lowest = TRUE )


OUT.LOCATION_quintile  = db.ALL %>% 
  group_by( LOCATION_quintile ) %>% 
  summarise(
    N_Cases = n(),
    mean =  round(mean(FA_FINAL_Fscores, na.rm = TRUE),2),
    min =  round(min(FA_FINAL_Fscores, na.rm = TRUE),2),
    max =  round(max(FA_FINAL_Fscores, na.rm = TRUE),2),
   
  )

OUT.LOCALITY_quintile  = db.ALL %>% 
  group_by( LOCALITY_quintile ) %>% 
  summarise(
    N_Cases = n(),
    mean =  round(mean(FA_COSCOMBINE_Fscores, na.rm = TRUE),2),
    min =  round(min(FA_COSCOMBINE_Fscores, na.rm = TRUE),2),
    max =  round(max(FA_COSCOMBINE_Fscores, na.rm = TRUE),2),
    
  )

OUT.POPCHANGE_quintile  = db.ALL %>% 
  group_by( POPCHANGE_quintile ) %>% 
  summarise(
    N_Cases = n(),
    mean =  round(mean(POP_VarPop_C2001C2011, na.rm = TRUE),2),
    min =  round(min(POP_VarPop_C2001C2011, na.rm = TRUE),2),
    max =  round(max(POP_VarPop_C2001C2011, na.rm = TRUE),2),
    
  )


####
# 2. AGRUPAMENTOS TRANSFORMACOES URBANAS V2 ----
####

#### * 2.1 CA POPCHANGE ####

CA_POPCHANGE = db.ALL[,c(
  "POP_VarPop_C2001C2011")
  ]


# Ward Hierarchical Clustering
d.CA_POPCHANGE  <- dist(CA_POPCHANGE, method = "euclidean") # distance matrix
fit.CA_POPCHANGE <- hclust(d.CA_POPCHANGE, method="ward.D")
plot(fit.CA_POPCHANGE) # display dendogram
CA_POPCHANGE_Ward4Groups <- cutree(fit.CA_POPCHANGE, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_POPCHANGE, k=4, border="red")


db.ALL = cbind(db.ALL, CA_POPCHANGE_Ward4Groups)

OUT.POPCHANGE_Ward4groups  = db.ALL %>% 
  group_by( CA_POPCHANGE_Ward4Groups ) %>% 
  summarise(
    CA_POPCHANGE = round(mean(POP_VarPop_C2001C2011, na.rm = TRUE),2)
  )


#### * 2.2 POPCHANGE 3Groups ####

hist(db.ALL$POP_VarPop_C2001C2011)
boxplot(db.ALL$POP_VarPop_C2001C2011)


quantile(db.ALL$POP_VarPop_C2001C2011)
db.ALL$POPCHANGE_3Groups = cut(db.ALL$POP_VarPop_C2001C2011, breaks = c(min(db.ALL$POP_VarPop_C2001C2011), -0.05, 0.05, max(db.ALL$POP_VarPop_C2001C2011) ), labels = c(1,2,3), include.lowest = T )



####
# 3. AGRUPAMENTOS TRANSFORMACOES URBANAS USING QUINTILES ----
####

#### * 3.1 - Clusters deductive POP CHANGE 2 CATEGORIES ####


#db.ALL$Metropolizacao = if_else( ( (db.ALL$CL_LOCALITY == 4 | db.ALL$CL_LOCALITY == 4) & (db.ALL$CL_LOCATION == 4 | db.ALL$CL_LOCATION == 4) & (db.ALL$CA_POPCHANGE_Ward4Groups) == 4), 1, 0)
#db.ALL[which(db.ALL$Metropolizacao == 1),c("DICOFRE18","FREG18_la.x")]

db.ALL$TU1_Urbanizacao = if_else( (db.ALL$CL_LOCALITY == 4 & (db.ALL$CL_LOCATION == 3 | db.ALL$CL_LOCATION == 4) &  db.ALL$POP_VarPop_C2001C2011 > 0) , 1, 0)
db.ALL[which(db.ALL$TU1_Urbanizacao == 1),c("DICOFRE18","FREG18_la.x")]

db.ALL$TU1_SubUrbanizacao = if_else( ( (db.ALL$CL_LOCALITY == 1 | db.ALL$CL_LOCALITY == 2) & (db.ALL$CL_LOCATION == 3) &  db.ALL$POP_VarPop_C2001C2011 > 0 ) , 1, 0)
db.ALL[which(db.ALL$TU1_SubUrbanizacao == 1),c("DICOFRE18","FREG18_la.x")]

db.ALL$TU1_PeriUrbanizacao = if_else( ( (db.ALL$CL_LOCALITY == 1 | db.ALL$CL_LOCALITY == 2) & (db.ALL$CL_LOCATION == 1 | db.ALL$CL_LOCATION == 2) &  db.ALL$POP_VarPop_C2001C2011 > 0 ) , 1, 0)
db.ALL[which(db.ALL$TU1_PeriUrbanizacao == 1),c("DICOFRE18","FREG18_la.x")]

db.ALL$TU1_DeclinioUrbano = if_else( ( (db.ALL$CL_LOCALITY == 2 | db.ALL$CL_LOCALITY == 4) & (db.ALL$CL_LOCATION == 3 | db.ALL$CL_LOCATION == 4) & db.ALL$POP_VarPop_C2001C2011 < 0 ) , 1, 0)
db.ALL[which(db.ALL$TU1_DeclinioUrbano == 1),c("DICOFRE18","FREG18_la.x")]

db.ALL$TU1_ContraUrbanizacao = if_else( ( (db.ALL$CL_LOCALITY == 3) & (db.ALL$CL_LOCATION == 1) & db.ALL$POP_VarPop_C2001C2011 > 0 ) , 1, 0)
db.ALL[which(db.ALL$TU1_ContraUrbanizacao == 1),c("DICOFRE18","FREG18_la.x")]

db.ALL$TU1_DeclinioRural = if_else( (db.ALL$CL_LOCALITY == 3 & (db.ALL$CL_LOCATION == 1) & db.ALL$POP_VarPop_C2001C2011 < 0 ) , 1, 0)
db.ALL[which(db.ALL$TU1_DeclinioRural == 1),c("DICOFRE18","FREG18_la.x")]


db.ALL$TU1Cod = "TUV1"

SEL_TU1 = c( "TU1_Urbanizacao",
"TU1_SubUrbanizacao",
"TU1_PeriUrbanizacao",
"TU1_DeclinioUrbano",
"TU1_ContraUrbanizacao",
"TU1_DeclinioRural")

db.ALL = unite(db.ALL, col="TU_VERSAO1", c(TU1Cod, TU1_Urbanizacao,
                           TU1_SubUrbanizacao,
                           TU1_PeriUrbanizacao,
                           TU1_DeclinioUrbano,
                           TU1_ContraUrbanizacao,
                           TU1_DeclinioRural), sep = "_", remove = F, na.rm = FALSE)

levels(as.factor(db.ALL$TU_VERSAO1))


OUT.TU1  = db.ALL %>% 
  group_by( TU_VERSAO1 ) %>% 
  summarise(
    N_Cases = n(),
    Locality_F_mean =  round(mean(FA_LOCALITY, na.rm = TRUE),2),
    Location_F_mean =  round(mean(FA_LOCATION, na.rm = TRUE),2),
    VarPop_mean =  round(mean(POP_VarPop_C2001C2011, na.rm = TRUE),2),
  )



db.all.store = db.ALL[,c("DICOFRE18",
                         
                         "TU_VERSAO1",
                         "FA_LOCALITY",
                          "FA_LOCATION",
                         "POP_VarPop_C2001C2011",
                         "CL_LOCALITY",
                         "CL_LOCATION"
                         )]

write_delim(db.all.store, paste(OUT.tables.path,"FREG_CAOP2018_TRANSF_TESTE_data3.txt", sep=""), quote_escape = "backslash")







####
# 0 SAVES  ----
####

write_delim(BD_TU_GLOBAL, paste(OUT.tables.path,"FREG_CAOP2018_TRANSFURB_GLOBAL_data.txt", sep=""), quote_escape = "backslash")

save(BD_TU_GLOBAL, OUT.TRANSF_URB,
     file = paste(OUT.RData.path, "FREG_CAOP2018_TRANSFURB_GLOBAL_data.RData", sep="") )





