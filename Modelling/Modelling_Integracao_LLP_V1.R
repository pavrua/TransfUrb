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


####
# 1. CLUSTERS NEW (6) ----
####

# 1.1 CLUSTERS FROM FACTORES ----

db.ALL = left_join(db.COS_COMBINE, db.ACESSIBILITY, by = c("DICOFRE18" = "DICOFRE18") )
db.ALL = left_join(db.ALL, OUT.POPCENSOS_2001e2011, by = c("DICOFRE18" = "DICOFRE18") )

CA_ORIF_Ward7 = db.ALL[,c(
  "FA_FINAL_Fscores", "FA_COSCOMBINE_Fscores", "POP_VarPop_C2001C2011")
  ]


# Ward Hierarchical Clustering
d.CA_ORIF_Ward7  <- dist(CA_ORIF_Ward7, method = "euclidean") # distance matrix
fit.CA_ORIF_Ward7 <- hclust(d.CA_ORIF_Ward7, method="ward.D")
plot(fit.CA_ORIF_Ward7) # display dendogram
CA_ORIF_Ward7_groups <- cutree(fit.CA_ORIF_Ward7, k=7) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_ORIF_Ward7, k=7, border="red")


db.COS_COMBINE = cbind(db.COS_COMBINE, CA_ORIF_Ward7_groups)

OUT.LOCALITY_CA_ORIF_Ward7_groups  = db.COS_COMBINE %>% 
  group_by( CA_ORIF_Ward7_groups ) %>% 
  summarise(
    Ward7_groups_n = n(),
    Locality_mean = round(mean(FA_COSCOMBINE_Fscores, na.rm = TRUE),2),
    Locality_max = round(max(FA_COSCOMBINE_Fscores, na.rm = TRUE),2),
    Locality_min = round(min(FA_COSCOMBINE_Fscores, na.rm = TRUE),2),
    
    Location_mean = round(mean(FA_FINAL_Fscores, na.rm = TRUE),2),
    Location_max = round(max(FA_FINAL_Fscores, na.rm = TRUE),2),
    Location_min = round(min(FA_FINAL_Fscores, na.rm = TRUE),2), 
    
    PopChange_mean = round(mean(POP_VarPop_C2001C2011, na.rm = TRUE),2),
    PopChange_max = round(max(POP_VarPop_C2001C2011, na.rm = TRUE),2),
    PopChange_min = round(min(POP_VarPop_C2001C2011, na.rm = TRUE),2),
    
    
  )







####
# 3. AGRUPAMENTOS TRANSFORMACOES URBANAS ----
####

SEL_LOCATION = c("DICOFRE18", 
                 "Pot_Acess_POIs_Educaca",
                  "Pot_Acess_POIS_Saud", 
                   "Pot_Acess_POIS_RestAli",
                   "Pot_Acess_POIS_ServGer"  ,
                   "Pot_Acess_POIS_Lazer",                  
                   "Pot_Acess_POIS_ServPubCom",
                   "PotDEMO2011",
                   "FA_FINAL_Fscores",                      
                   "fit_CA_AF_FINAL_groups")  

SEL_LOCALITY = c( "DICOFRE18",
                  "PropT111",
                  "PropT112",
                  "PropT121",
                  "PropT122",
                  "PropT141",
                  "PopDens",
                  "PopDensByT1",
                  "FA_COSCOMBINE_Fscores",
                  "CA_COS_COMBINE_F0_Wardgroups")

SEL_POP = c(  "DICOFRE18", "FREG18_la",
              "POP_C2001_Total", 
              "POP_C2011_Total",
              "POP_VarPop_C2001C2011")


BD_TU_GLOBAL = left_join(OUT.POPCENSOS_2001e2011[,SEL_POP], Acess_BD_withAFeCA_FINAL[,SEL_LOCATION], by = c("DICOFRE18", "DICOFRE18") )
BD_TU_GLOBAL = left_join(BD_TU_GLOBAL, db.COS_COMBINE[,SEL_LOCALITY], by = c("DICOFRE18", "DICOFRE18") )

BD_TU_GLOBAL$CL_LOCALITY = BD_TU_GLOBAL$CA_COS_COMBINE_F0_Wardgroups
BD_TU_GLOBAL$CL_LOCATION = BD_TU_GLOBAL$fit_CA_AF_FINAL_groups

#### * 4.1 - Clusters deductive ####

BD_TU_GLOBAL$TRANSF_URB_V1 <- NA
BD_TU_GLOBAL$TRANSF_URB_V1 <- factor(BD_TU_GLOBAL$TRANSF_URB_V1, levels = c(1,2,3,4,5, 6, 7))

BD_TU_GLOBAL[(BD_TU_GLOBAL$CL_LOCALITY == 4 & BD_TU_GLOBAL$CL_LOCATION == 4 & BD_TU_GLOBAL$POP_VarPop_C2001C2011 >= 0) , "TRANSF_URB_V1"] <- "1"
BD_TU_GLOBAL[(BD_TU_GLOBAL$CL_LOCALITY == 2 & BD_TU_GLOBAL$CL_LOCATION == 3 &  BD_TU_GLOBAL$POP_VarPop_C2001C2011 >= 0 ) , "TRANSF_URB_V1"] <- "2"
BD_TU_GLOBAL[( (BD_TU_GLOBAL$CL_LOCALITY == 1 | BD_TU_GLOBAL$CL_LOCALITY == 2) & (BD_TU_GLOBAL$CL_LOCATION == 3 | BD_TU_GLOBAL$CL_LOCATION == 4) & BD_TU_GLOBAL$POP_VarPop_C2001C2011 >= 0 ) , "TRANSF_URB_V1"] <- "3"
BD_TU_GLOBAL[( (BD_TU_GLOBAL$CL_LOCALITY == 2 | BD_TU_GLOBAL$CL_LOCALITY == 4) & BD_TU_GLOBAL$POP_VarPop_C2001C2011 < 0 ) , "TRANSF_URB_V1"] <- "4"
BD_TU_GLOBAL[( (BD_TU_GLOBAL$CL_LOCALITY == 1 | BD_TU_GLOBAL$CL_LOCALITY == 3) & (BD_TU_GLOBAL$CL_LOCATION == 1 | BD_TU_GLOBAL$CL_LOCATION == 2) & BD_TU_GLOBAL$POP_VarPop_C2001C2011 >= 0) , "TRANSF_URB_V1"] <- "5"
BD_TU_GLOBAL[(BD_TU_GLOBAL$CL_LOCALITY == 3 & BD_TU_GLOBAL$CL_LOCATION == 1 & BD_TU_GLOBAL$POP_VarPop_C2001C2011 < 0) , "TRANSF_URB_V1"] <- "6"

BD_TU_GLOBAL$TRANSF_URB_V1[is.na(BD_TU_GLOBAL$TRANSF_URB_V1)] = "7"



OUT.TRANSF_URB  = BD_TU_GLOBAL %>% 
  group_by( TRANSF_URB_V1 ) %>% 
  summarise(
    N_Cases = n(),
    Locality_F_mean =  round(mean(FA_COSCOMBINE_Fscores, na.rm = TRUE),2),
    Location_F_mean =  round(mean(FA_FINAL_Fscores, na.rm = TRUE),2),
    VarPop_mean =  round(mean(POP_VarPop_C2001C2011, na.rm = TRUE),2),
  )


####
# 0 SAVES  ----
####

write_delim(BD_TU_GLOBAL, paste(OUT.tables.path,"FREG_CAOP2018_TRANSFURB_GLOBAL_data.txt", sep=""), quote_escape = "backslash")

save(BD_TU_GLOBAL, OUT.TRANSF_URB,
     file = paste(OUT.RData.path, "FREG_CAOP2018_TRANSFURB_GLOBAL_data.RData", sep="") )





