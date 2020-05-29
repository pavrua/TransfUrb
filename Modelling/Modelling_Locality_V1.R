####
# | Criado em: 2020-05-25 |
# PROCESSOS TRANSFORMACAO URBBANA
# MODELACAO «LOCALITY» Versao 01 | 2020-05-25   ----
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


CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")
CAOP2018_FREG_Descodifica = CAOP2018_FREG_Descodifica.shp %>% st_drop_geometry()

#DADOS DEMOGRAFICOS (para potencial demografico)
#FICHEIROS: BD_DEMOALL, OUT.POPCENSOS_2001e2011, OUT.POTDEMO2011byFREG2018, OUT.POP_PPF
load( paste(OUT.RData.path,"DEMOGRAFIA_01_11_15_CAOP2018.RData", sep="") )

#DADOS COS2015 (para ocupação do solo)
#FICHEIROS: COS2015withCAOP2018_BDALL, OUT.COS2015withCAOP2018F_N1_T0.wide, 
# OUT.COS2015withCAOP2018F_N12_T1.wide, OUT.COS2015withCAOP2018F_N123_T1.wide, 
# OUT.COS2015_categorias
load( paste(OUT.RData.path,"COS2015N123T1234T1byCAOP2018.RData", sep="") )



####
# 1 DEMOGRAPHIC DENSITY  ----
####

COSandPOP = left_join(OUT.COS2015withCAOP2018F_N1_T0.wide, OUT.POPCENSOS_2001e2011, by = c("DICOFRE18", "DICOFRE18") )

COSandPOP$PopDensByT1 = COSandPOP$POP_C2011_Total / COSandPOP$`1`*10^6
COSandPOP$PopDens = COSandPOP$POP_C2011_Total / COSandPOP$AREAFREG18_N1*10^6

####
# 2 PROP COS T1 N123  ----
####

COS_N0T0 = left_join(OUT.COS2015withCAOP2018F_N1_T0.wide, OUT.COS2015withCAOP2018F_N123_T1.wide, by = c("DICOFRE18", "DICOFRE18") )

COS_N0T0$PropT111 = COS_N0T0$`1_1_1` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT112 = COS_N0T0$`1_1_2` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT121 = COS_N0T0$`1_2_1` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT122 = COS_N0T0$`1_2_2` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT123 = COS_N0T0$`1_2_3` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT124 = COS_N0T0$`1_2_4` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT131 = COS_N0T0$`1_3_1` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT132 = COS_N0T0$`1_3_2` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT133 = COS_N0T0$`1_3_3` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT141 = COS_N0T0$`1_4_1` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT142 = COS_N0T0$`1_4_2` / COS_N0T0$AREAFREG18_N1

COS_N0T0$PropT1 = COS_N0T0$`1` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT2 = COS_N0T0$`2` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT3 = COS_N0T0$`3` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT4 = COS_N0T0$`4` / COS_N0T0$AREAFREG18_N1
COS_N0T0$PropT5 = COS_N0T0$`5` / COS_N0T0$AREAFREG18_N1


COS_N0T0[is.na(COS_N0T0)] <- 0


####
# 3 AF COS  ----
####


dataFA_COS = COS_N0T0[,c(
  "PropT111",  "PropT112",
  "PropT121",  "PropT122",  
  "PropT141" 
  
  )
  ]

# "PropT123" "PropT131" "PropT132" "PropT133" "PropT142", "PropT124", "PropT4","PropT5" "PropT2","PropT3"

fit.FA_COS <- prcomp(dataFA_COS, scale = TRUE)
fit.FA_COS
fit.FA_COS$rotation # pc loadings


fit.FA_COSb <- principal(dataFA_COS, nfactors=1, rotate="varimax")
fit.FA_COSb # print results 



# 3.1 AF COS COMBINE  ----

COS_N0T0_sel = c("DICOFRE18", "PropT111",  "PropT112",
  "PropT121",  "PropT122",  
  "PropT141" )

COS_COMBINE = left_join(COS_N0T0[,COS_N0T0_sel], COSandPOP[,c("DICOFRE18", "PopDens", "PopDensByT1")], by = c("DICOFRE18", "DICOFRE18") )



dataFA_COS_COMBINE = COS_COMBINE[,c(
  "PropT111",  "PropT112",
  "PropT121",  "PropT122",  
  "PropT141",
  "PopDens",
  "PopDensByT1"
  )]

fit.COS_COMBINE <- prcomp(dataFA_COS_COMBINE, scale = TRUE)
fit.COS_COMBINE
fit.COS_COMBINE$rotation # pc loadings


fit.COS_COMBINEb <- principal(dataFA_COS_COMBINE, nfactors=1, rotate="varimax")
fit.COS_COMBINEb # print results 

fit.COS_COMBINE_2Fb <- principal(dataFA_COS_COMBINE, nfactors=1, rotate="varimax")
fit.COS_COMBINE_2Fb # print results 

COS_COMBINE = cbind(COS_COMBINE, fit.COS_COMBINE_2Fb$scores)
colnames(COS_COMBINE)[ncol(COS_COMBINE)] = c("FA_COSCOMBINE_Fscores")

####
# 4 CLUSTER ANALYSIS  ----
####

#### * 4.1 CA AF F0 ####

CA_COS_COMBINE_F0 = COS_COMBINE[,c(
  "FA_COSCOMBINE_Fscores")
  ]


# Ward Hierarchical Clustering
d.CA_COS_COMBINE_F0  <- dist(CA_COS_COMBINE_F0, method = "euclidean") # distance matrix
fit.CA_COS_COMBINE_F0 <- hclust(d.CA_COS_COMBINE_F0, method="ward.D")
plot(fit.CA_COS_COMBINE_F0) # display dendogram
CA_COS_COMBINE_F0_Wardgroups <- cutree(fit.CA_COS_COMBINE_F0, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_COS_COMBINE_F0, k=4, border="red")


COS_COMBINE = cbind(COS_COMBINE, CA_COS_COMBINE_F0_Wardgroups)

OUT.LOCALITY_CA_Ward_AF_F0  = COS_COMBINE %>% 
  group_by( CA_COS_COMBINE_F0_Wardgroups ) %>% 
  summarise(
    CA_AF_F0 = round(mean(FA_COSCOMBINE_Fscores, na.rm = TRUE),2)
  )

# Clusters / Groups by quantile
hist(COS_COMBINE$FA_COSCOMBINE_Fscores)
boxplot(COS_COMBINE$FA_COSCOMBINE_Fscores)

quantile(COS_COMBINE$FA_COSCOMBINE_Fscores)
COS_COMBINE$CA_COS_COMBINE_F0_quantile = cut(COS_COMBINE$FA_COSCOMBINE_Fscores, quantile(COS_COMBINE$FA_COSCOMBINE_Fscores), labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE )

# Clusters / Groups by jeanks
getJenksBreaks(COS_COMBINE$FA_COSCOMBINE_Fscores,5)
COS_COMBINE$CA_COS_COMBINE_F0_jenks = cut(COS_COMBINE$FA_COSCOMBINE_Fscores, getJenksBreaks(COS_COMBINE$FA_COSCOMBINE_Fscores,5), labels = c("jenks1", "jenks2", "jenks3", "jenks4"), include.lowest = TRUE )






####
# 0 SAVES  ----
####

write_delim(COS_COMBINE, paste(OUT.tables.path,"FREG_CAOP2018_Locality_data.txt", sep=""), quote_escape = "backslash")


db.COS_COMBINE = COS_COMBINE
save(db.COS_COMBINE, OUT.LOCALITY_CA_Ward_AF_F0,
     file = paste(OUT.RData.path, "FREG_CAOP2018_Locality_data.RData", sep="") )



