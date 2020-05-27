####
# | Criado em: 2020-05-10 |
# PROCESSOS TRANSFORMACAO URBBANA
# MODELACAO «Location» Versao 01 | 2020-05-10   ----
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

#library("ggfortify")
library("BAMMtools")

#install.packages("arcgisbinding", repos="http://r-arcgis.github.io/r-bridge", type="win.binary")
#library("arcgisbinding")

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

ArcGIS_BRIDGE = paste(path, "01_BASESDEDADOS/SIG/Z_ArcGIS_BRIDGE", sep = "")


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
# 1 DEMOGRAPHIC POTENTIAL  ----
####

POP2011byFREG2018 = OUT.POPCENSOS_2001e2011[,c("DICOFRE18", "POP_C2011_Total")]

FREG18_DistMatrixLabels_WiderF_MatrixTransf = FREG18_DistMatrixLabels_WiderF_Matrix*10^-3
FREG18_DistMatrixLabels_WiderF_MatrixTransf = (FREG18_DistMatrixLabels_WiderF_MatrixTransf)^2
FREG18_DistMatrixLabels_WiderF_MatrixTransf = 1/FREG18_DistMatrixLabels_WiderF_MatrixTransf

POTDEMO2011byFREG2018_A =  t(as.matrix(POP2011byFREG2018[,2])) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf   

POTDEMO2011byFREG2018 = data.frame(DICOFRE18 = colnames(POTDEMO2011byFREG2018_A), PotDEMO2011 = as.numeric(t(POTDEMO2011byFREG2018_A))  )




####
# 2 ACESSIBILITY   ----
####

POIsCATEGORIES = read.xlsx( file = paste(OUT.tables.path, "POIs_categorizar.xlsx", sep=""), "POIS_proposta_nova")
colnames(POIsCATEGORIES)

# * 2.1 Select Tags by theme   ----
POIStag_Educ = subset(POIsCATEGORIES, Educ == 1, c("tag"))
POIStag_Saud = subset(POIsCATEGORIES, Saud == 1, c("tag"))
POIStag_RestAli = subset(POIsCATEGORIES, RestAli == 1, c("tag"))
POIStag_ServGer = subset(POIsCATEGORIES, ServGer == 1, c("tag"))
POIStag_Lazer = subset(POIsCATEGORIES, Lazer == 1, c("tag"))
POIStag_ServPubCom = subset(POIsCATEGORIES, ServPubCom == 1, c("tag"))
POIStag_Global = subset(POIsCATEGORIES,  Global == 1, c("tag"))


# * 2.2 Count POIs by FREG2018   ----

all.dados.sig.withoutNA.CAOP2018.unique.F = all.dados.sig.withoutNA.CAOP2018.unique.F %>% st_drop_geometry()

# * * 2.2.1 Acess_POIs_Global   ----
Acess_POIs_Global = filter(all.dados.sig.withoutNA.CAOP2018.unique.F, tag %in% POIStag_Global$tag )
Acess_POIs_Global_byCAOP2018 = Acess_POIs_Global %>% 
  group_by( DICOFRE18, ) %>% 
  summarise(
    N_Acess_POIs_Global = n()
  )

# * * 2.2.2 Acess_POIS_Educ   ----
Acess_POIS_Educ = filter(all.dados.sig.withoutNA.CAOP2018.unique.F, tag %in% POIStag_Educ$tag )
Acess_POIS_Educ_byCAOP2018 = Acess_POIS_Educ %>% 
  group_by( DICOFRE18, ) %>% 
  summarise(
    N_Acess_POIs_Educ = n()
  )

# * * 2.2.3 Acess_POIS_Saud   ----
Acess_POIS_Saud = filter(all.dados.sig.withoutNA.CAOP2018.unique.F, tag %in% POIStag_Saud$tag )
Acess_POIS_Saud_byCAOP2018 = Acess_POIS_Saud %>% 
  group_by( DICOFRE18, ) %>% 
  summarise(
    N_Acess_POIS_Saud = n()
  )

# * * 2.2.4 Acess_POIS_RestAli   ----
Acess_POIS_RestAli = filter(all.dados.sig.withoutNA.CAOP2018.unique.F, tag %in% POIStag_RestAli$tag )
Acess_POIS_RestAli_byCAOP2018 = Acess_POIS_RestAli %>% 
  group_by( DICOFRE18, ) %>% 
  summarise(
    N_Acess_POIS_RestAli = n()
  )

# * * 2.2.5 Acess_POIS_ServGer   ----
Acess_POIS_ServGer = filter(all.dados.sig.withoutNA.CAOP2018.unique.F, tag %in% POIStag_ServGer$tag )
Acess_POIS_ServGer_byCAOP2018 = Acess_POIS_ServGer %>% 
  group_by( DICOFRE18, ) %>% 
  summarise(
    N_Acess_POIS_ServGer = n()
  )

# * * 2.2.6 Acess_POIS_Lazer   ----
Acess_POIS_Lazer = filter(all.dados.sig.withoutNA.CAOP2018.unique.F, tag %in% POIStag_Lazer$tag )
Acess_POIS_Lazer_byCAOP2018 = Acess_POIS_Lazer %>% 
  group_by( DICOFRE18, ) %>% 
  summarise(
    N_Acess_POIS_Lazer = n()
  )

# * * 2.2.7 Acess_POIS_ServPubCom   ----
Acess_POIS_ServPubCom = filter(all.dados.sig.withoutNA.CAOP2018.unique.F, tag %in% POIStag_ServPubCom$tag )
Acess_POIS_ServPubCom_byCAOP2018 = Acess_POIS_ServPubCom %>% 
  group_by( DICOFRE18, ) %>% 
  summarise(
    N_Acess_POIS_ServPubCom = n()
  )

#foo <- mapvalues(foo, from=c("AA", "AC", "AG"), to=c("0101", "0102", "0103"))

# * 2.3 Acess All BD   ----

Acess_BD = left_join(CAOP2018_FREG_Descodifica, Acess_POIs_Global_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18") )
Acess_BD = left_join(Acess_BD, Acess_POIS_Educ_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18") )
Acess_BD = left_join(Acess_BD, Acess_POIS_Saud_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18") )
Acess_BD = left_join(Acess_BD, Acess_POIS_RestAli_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18") )
Acess_BD = left_join(Acess_BD, Acess_POIS_ServGer_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18") )
Acess_BD = left_join(Acess_BD, Acess_POIS_Lazer_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18") )
Acess_BD = left_join(Acess_BD, Acess_POIS_ServPubCom_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18") )

Acess_BD = left_join(Acess_BD, POP2011byFREG2018, by = c("DICOFRE18" = "DICOFRE18") )

Acess_BD[is.na(Acess_BD)] <- 0

Acess_BD = mutate_at(Acess_BD, 
                      c("N_Acess_POIs_Global",
                        "N_Acess_POIs_Educ",
                        "N_Acess_POIS_Saud",
                        "N_Acess_POIS_RestAli",
                        "N_Acess_POIS_ServGer",
                        "N_Acess_POIS_Lazer",
                        "N_Acess_POIS_ServPubCom"),
                      funs(POIsNDensPop = ./POP_C2011_Total))


####
# 3 ACESSIBILITY POTENTIAL  ----
####



Acess_BD$Pot_Acess_POIs_Global = t( t(as.matrix(Acess_BD$N_Acess_POIs_Global)) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf )
Acess_BD$Pot_Acess_POIs_Educaca = t( t(as.matrix(Acess_BD$N_Acess_POIs_Educ)) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf )
Acess_BD$Pot_Acess_POIS_Saud = t( t(as.matrix(Acess_BD$N_Acess_POIS_Saud)) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf )
Acess_BD$Pot_Acess_POIS_RestAli = t( t(as.matrix(Acess_BD$N_Acess_POIS_RestAli)) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf )
Acess_BD$Pot_Acess_POIS_ServGer = t( t(as.matrix(Acess_BD$N_Acess_POIS_ServGer)) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf )
Acess_BD$Pot_Acess_POIS_Lazer = t( t(as.matrix(Acess_BD$N_Acess_POIS_Lazer)) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf )
Acess_BD$Pot_Acess_POIS_ServPubCom = t( t(as.matrix(Acess_BD$N_Acess_POIS_ServPubCom)) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf )

Acess_BD = mutate_at(Acess_BD, 
                      c("Pot_Acess_POIs_Global",
                                  "Pot_Acess_POIs_Educaca",
                                  "Pot_Acess_POIS_Saud",
                                  "Pot_Acess_POIS_RestAli",
                                  "Pot_Acess_POIS_ServGer",
                                  "Pot_Acess_POIS_Lazer",
                                  "Pot_Acess_POIS_ServPubCom"),
                      funs(standardized = scale) )


####
# 4 FACTOR ANALYSIS  ----
####

Acess_BD_withAF = Acess_BD

#### * 4.1 AF A (ACESS) ####

#### * * 4.1.1 AF_A1 Acess Potential (only)  ####

# Factor analysis: https://www.statmethods.net/advstats/factor.html

dataFA_AcessPotent = Acess_BD[,c(
                        "Pot_Acess_POIs_Educaca",
                        "Pot_Acess_POIS_Saud",
                        "Pot_Acess_POIS_RestAli",
                        "Pot_Acess_POIS_ServGer",
                        "Pot_Acess_POIS_Lazer",
                        "Pot_Acess_POIS_ServPubCom")
                     ]

fit.FA_AcessPotent <- prcomp(dataFA_AcessPotent, scale = TRUE)
fit.FA_AcessPotent
fit.FA_AcessPotent$rotation # pc loadings


fit.FA_AcessPotentb <- principal(dataFA_AcessPotent, nfactors=1, rotate="varimax")
fit.FA_AcessPotentb # print results 

Acess_BD_withAF = cbind(Acess_BD_withAF, fit.FA_AcessPotentb$scores)
colnames(Acess_BD_withAF)[ncol(Acess_BD_withAF)] = "FA_AcessPot_scores"



#### * * 4.1.2 AF_A2 Acess N (only)  ####

# Factor analysis: https://www.statmethods.net/advstats/factor.html

# Factor analysis: https://www.statmethods.net/advstats/factor.html

dataFA_AcessN = Acess_BD[,c(
  "N_Acess_POIs_Educ",                                    
  "N_Acess_POIS_Saud",                                       
  "N_Acess_POIS_RestAli",                     
  "N_Acess_POIS_ServGer",                              
  "N_Acess_POIS_Lazer",                
  "N_Acess_POIS_ServPubCom")
  ]

fit.FA_AcessN <- prcomp(dataFA_AcessN, scale = TRUE)
summary(fit.FA_AcessN) # print variance accounted for
fit.FA_AcessN$rotation # pc loadings


# Determine Number of Factors to Extract
#ev <- eigen(cor(dataFA_AP)) # get eigenvalues
#ap <- parallel(subject=nrow(dataFA_AP),var=ncol(dataFA_AP),
#               rep=100,cent=.05)
#nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
#plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components

fit.FA_AcessNb <- principal(dataFA_AcessN, nfactors=1, rotate="varimax")
fit.FA_AcessNb # print results 

Acess_BD_withAF = cbind(Acess_BD_withAF, fit.FA_AcessNb$scores)
colnames(Acess_BD_withAF)[ncol(Acess_BD_withAF)] = "FA_AcessN_scores"


#### * * 4.1.3 AF_A3 Acess N by DensPOP  ####

# Factor analysis: https://www.statmethods.net/advstats/factor.html

dataFA_AcessNDensPop = Acess_BD[,c(
  "N_Acess_POIs_Educ_POIsNDensPop",                       
  "N_Acess_POIS_Saud_POIsNDensPop",                          
  "N_Acess_POIS_RestAli_POIsNDensPop",        
  "N_Acess_POIS_ServGer_POIsNDensPop",                 
  "N_Acess_POIS_Lazer_POIsNDensPop",   
  "N_Acess_POIS_ServPubCom_POIsNDensPop" )
  ]

fit.FA_AcessNDensPop <- prcomp(dataFA_AcessNDensPop, scale = TRUE)
fit.FA_AcessNDensPop
summary(fit.FA_AcessNDensPop) # print variance accounted for
fit.FA_AcessNDensPop$rotation # pc loadings


# Determine Number of Factors to Extract
#ev <- eigen(cor(dataFA_AP)) # get eigenvalues
#ap <- parallel(subject=nrow(dataFA_AP),var=ncol(dataFA_AP),
#               rep=100,cent=.05)
#nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
#plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components

fit.FA_AcessNDensPopb <- principal(dataFA_AcessNDensPop, nfactors=2, rotate="varimax")
fit.FA_AcessNDensPopb # print results 

Acess_BD_withAF = cbind(Acess_BD_withAF, fit.FA_AcessNDensPopb$scores)
colnames(Acess_BD_withAF)[41:42] = c("FA_AcessNDensPop_scores_F1", "FA_AcessNDensPop_scores_F2")




#### * 4.2 AF B ( ACESS + DEMO)  ####

#### * * 4.2.1 AF_B1 Global :: AF Acess  Potential & PotencialDemo   ####

# Factor analysis: https://www.statmethods.net/advstats/factor.html

Acess_BD_withAF = left_join(Acess_BD_withAF, POTDEMO2011byFREG2018, by = c("DICOFRE18" = "DICOFRE18") )


dataFA_Global21 = Acess_BD_withAF[,c(
  "PotDEMO2011",                                    
  "FA_AcessPot_scores")
  ]

fit.FA_Global21 <- prcomp(dataFA_Global21, scale = TRUE)
fit.FA_Global21 # print variance accounted for
fit.FA_Global21$rotation # pc loadings


# Determine Number of Factors to Extract
#ev <- eigen(cor(dataFA_AP)) # get eigenvalues
#ap <- parallel(subject=nrow(dataFA_AP),var=ncol(dataFA_AP),
#               rep=100,cent=.05)
#nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
#plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components

fit.FA_Global21b <- principal(dataFA_Global21, nfactors=1, rotate="varimax")
fit.FA_Global21b # print results 

Acess_BD_withAF = cbind(Acess_BD_withAF, fit.FA_Global21b$scores)
colnames(Acess_BD_withAF)[ncol(Acess_BD_withAF)] = "FA_Global21b_scores"



#### * * 4.2.2 AF_B2 Global :: AF Acess N & PotencialDemo   ####

# Factor analysis: https://www.statmethods.net/advstats/factor.html


dataFA_Global22 = Acess_BD_withAF[,c(
  "PotDEMO2011",                                    
  "FA_AcessN_scores")
  ]

fit.FA_Global22 <- prcomp(dataFA_Global22, scale = TRUE)
fit.FA_Global22 # print variance accounted for
fit.FA_Global22$rotation # pc loadings


# Determine Number of Factors to Extract
#ev <- eigen(cor(dataFA_AP)) # get eigenvalues
#ap <- parallel(subject=nrow(dataFA_AP),var=ncol(dataFA_AP),
#               rep=100,cent=.05)
#nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
#plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components

fit.FA_Global22b <- principal(dataFA_Global22, nfactors=1, rotate="varimax")
fit.FA_Global22b # print results 

Acess_BD_withAF = cbind(Acess_BD_withAF, fit.FA_Global22b$scores)
colnames(Acess_BD_withAF)[ncol(Acess_BD_withAF)] = "FA_Global22b_scores"


#### * * 4.2.3 AF_B3 Global :: AF Acess N & PotencialDemo   ####

# Factor analysis: https://www.statmethods.net/advstats/factor.html


dataFA_Global23 = Acess_BD_withAF[,c(
  "PotDEMO2011",                                    
  "FA_AcessNDensPop_scores_F1", "FA_AcessNDensPop_scores_F2")
  ]

fit.FA_Global23 <- prcomp(dataFA_Global23, scale = TRUE)
fit.FA_Global23 # print variance accounted for
fit.FA_Global23$rotation # pc loadings


# Determine Number of Factors to Extract
#ev <- eigen(cor(dataFA_AP)) # get eigenvalues
#ap <- parallel(subject=nrow(dataFA_AP),var=ncol(dataFA_AP),
#               rep=100,cent=.05)
#nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
#plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components

fit.FA_Global23b <- principal(dataFA_Global23, nfactors=2, rotate="varimax")
fit.FA_Global23b # print results 
fit.FA_Global23b$loadings

Acess_BD_withAF = cbind(Acess_BD_withAF, fit.FA_Global23b$scores)
colnames(Acess_BD_withAF)[45:46] = c("FA_Global23_scores_F1", "FA_Global23_scores_F2")

Acess_BD_withAF$FA_Global23_scores_F0 = ( (fit.FA_Global23b$values[1] * Acess_BD_withAF$FA_Global23_scores_F1) + (fit.FA_Global23b$values[2] * Acess_BD_withAF$FA_Global23_scores_F2) ) / (fit.FA_Global23b$values[1] + fit.FA_Global23b$values[2])




####
# 5 CLUSTER ANALYSIS  ----
####

Acess_BD_withAFeAC = Acess_BD_withAF
Acess_BD_withAFeAC = mutate_at(Acess_BD_withAFeAC, 
                     vars(c("PotDEMO2011")),
                     funs(PotDEMO2011_stand = scale) )


#### * 5.1 CA AF B1 ####

data.CA_AFB1 = Acess_BD_withAFeAC[,c(
  "FA_Global21b_scores")
  ]


# Ward Hierarchical Clustering
d.CA_AFB1 <- dist(data.CA_AFB1, method = "euclidean") # distance matrix
fit.CA_AFB1 <- hclust(d.CA_AFB1, method="ward.D")
plot(fit.CA_AFB1) # display dendogram
fit_CA_AFB1_groups <- cutree(fit.CA_AFB1, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_AFB1, k=4, border="red")


Acess_BD_withAFeAC = cbind(Acess_BD_withAFeAC, fit_CA_AFB1_groups)

OUT.fit_CA_AFB1_groups = Acess_BD_withAFeAC %>% 
  group_by( fit_CA_AFB1_groups ) %>% 
  summarise(
    
    dC1_CA_AFB1 = round(mean(FA_Global21b_scores, na.rm = TRUE),2)
    
  )

# Clusters / Groups by quantile
hist(Acess_BD_withAFeAC$FA_Global21b_scores)
boxplot(Acess_BD_withAFeAC$FA_Global21b_scores)

quantile(Acess_BD_withAFeAC$FA_Global21b_scores)

Acess_BD_withAFeAC$fit_CA_AFB1_groups_qtl = cut(Acess_BD_withAFeAC$FA_Global21b_scores, quantile(Acess_BD_withAFeAC$FA_Global21b_scores), labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE )


# Clusters / Groups by jeanks
getJenksBreaks(Acess_BD_withAFeAC$FA_Global21b_scores,5)

Acess_BD_withAFeAC$fit_CA_AFB1_groups_jenks = cut(Acess_BD_withAFeAC$FA_Global21b_scores, getJenksBreaks(Acess_BD_withAFeAC$FA_Global21b_scores,5), labels = c("jenks1", "jenks2", "jenks3", "jenks4"), include.lowest = TRUE )


#### * 5.2 CA AF B3  ####

#### * 5.2.1 CA AF B31  ####

data.CA_AFB31 = Acess_BD_withAFeAC[,c(
  "FA_Global23_scores_F1",
  "FA_Global23_scores_F2")
  ]


# Ward Hierarchical Clustering
d.CA_AFB31 <- dist(data.CA_AFB31, method = "euclidean") # distance matrix
fit.CA_AFB31 <- hclust(d.CA_AFB31, method="ward.D")
plot(fit.CA_AFB31) # display dendogram
fit_CA_AFB31_groups <- cutree(fit.CA_AFB31, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_AFB31, k=4, border="red")


Acess_BD_withAFeAC = cbind(Acess_BD_withAFeAC, fit_CA_AFB31_groups)

OUT.fit_CA_AFB31_groups = Acess_BD_withAFeAC %>% 
  group_by( fit_CA_AFB31_groups ) %>% 
  summarise(
    
    dC1_CA_AFB31 = round(mean(FA_Global23_scores_F1, na.rm = TRUE),2),
    dC2_CA_AFB31 = round(mean(FA_Global23_scores_F2, na.rm = TRUE),2)
    
  )


#### * 5.2.2 CA AF B31b  ####

data.CA_AFB31b = Acess_BD_withAFeAC[,c(
  "FA_Global23_scores_F0")
  ]


# Ward Hierarchical Clustering
d.CA_AFB31b <- dist(data.CA_AFB31b, method = "euclidean") # distance matrix
fit.CA_AFB31b <- hclust(d.CA_AFB31b, method="ward.D")
plot(fit.CA_AFB31b) # display dendogram
fit_CA_AFB31b_groups <- cutree(fit.CA_AFB31b, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_AFB31b, k=4, border="red")


Acess_BD_withAFeAC = cbind(Acess_BD_withAFeAC, fit_CA_AFB31b_groups)


OUT.fit_CA_AFB31b_groups = Acess_BD_withAFeAC %>% 
  group_by( fit_CA_AFB31b_groups ) %>% 
  summarise(
    dC1_CA_AFB31b = round(mean(FA_Global23_scores_F1, na.rm = TRUE),2)
  )


# Clusters / Groups by quantile
hist(Acess_BD_withAFeAC$FA_Global23_scores_F0)
boxplot(Acess_BD_withAFeAC$FA_Global23_scores_F0)

quantile(Acess_BD_withAFeAC$FA_Global23_scores_F0)

Acess_BD_withAFeAC$fit_CA_AFB31b_groups_qtl = cut(Acess_BD_withAFeAC$FA_Global23_scores_F0, quantile(Acess_BD_withAFeAC$FA_Global23_scores_F0), labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE )

OUT.fit_CA_AFB31b_qtl = Acess_BD_withAFeAC %>% 
  group_by( fit_CA_AFB31b_groups_qtl ) %>% 
  summarise(
    dC1_CA_AFB31b = round(mean(FA_Global23_scores_F0, na.rm = TRUE),2)
  )


# Clusters / Groups by jeanks
getJenksBreaks(Acess_BD_withAFeAC$FA_Global23_scores_F0,5)

Acess_BD_withAFeAC$fit_CA_AFB31b_groups_jenks = cut(Acess_BD_withAFeAC$FA_Global23_scores_F0, getJenksBreaks(Acess_BD_withAFeAC$FA_Global23_scores_F0,5), labels = c("jenks1", "jenks2", "jenks3", "jenks4"), include.lowest = TRUE )






#### * 5.3 CA Others ####

#### * * 5.3.1 CA_All ####

data.CA_All = Acess_BD_withAFeAC[,c(
  "PotDEMO2011_stand",                      
  "Pot_Acess_POIs_Educaca_standardized",                      
  "Pot_Acess_POIS_Saud_standardized",                        
  "Pot_Acess_POIS_RestAli_standardized",      
  "Pot_Acess_POIS_ServGer_standardized",               
  "Pot_Acess_POIS_Lazer_standardized", 
  "Pot_Acess_POIS_ServPubCom_standardized")
  ]


# Ward Hierarchical Clustering
d.CA_All <- dist(data.CA_All, method = "euclidean") # distance matrix
fit.CA_All <- hclust(d.CA_All, method="ward.D")
plot(fit.CA_All) # display dendogram
fit_CA_All_groups <- cutree(fit.CA_All, k=7) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_All, k=7, border="red")


Acess_BD_withAFeAC = cbind(Acess_BD_withAFeAC, fit_CA_All_groups)

OUT.fit.CA_All_groups = Acess_BD_withAFeAC %>% 
  group_by( fit_CA_All_groups ) %>% 
  summarise(
    
    dC1 = round(mean(PotDEMO2011_stand, na.rm = TRUE),2),
    dC2 = round(mean(Pot_Acess_POIs_Educaca_standardized, na.rm = TRUE),2),
    dC3 = round(mean(Pot_Acess_POIS_Saud_standardized, na.rm = TRUE),2),
    dC4 = round(mean(Pot_Acess_POIS_RestAli_standardized, na.rm = TRUE),2),
    dC5 = round(mean(Pot_Acess_POIS_ServGer_standardized, na.rm = TRUE),2),
    dC6 = round(mean(Pot_Acess_POIS_Lazer_standardized, na.rm = TRUE),2),
    dC7 = round(mean(Pot_Acess_POIS_ServPubCom_standardized, na.rm = TRUE),2)
    
  )



#### * * 5.3.2 CA_2F ####

data.CA_2F = Acess_BD_withAFeAC[,c(
  "PotDEMO2011_stand",                      
  "FA_AcessPot_scores")
  ]


# Ward Hierarchical Clustering
d.CA_2F <- dist(data.CA_2F, method = "euclidean") # distance matrix
fit.CA_2F <- hclust(d.CA_2F, method="ward.D")
plot(fit.CA_2F) # display dendogram
fit_CA_2F_groups <- cutree(fit.CA_2F, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.CA_2F, k=5, border="red")


Acess_BD_withAFeAC = cbind(Acess_BD_withAFeAC, fit_CA_2F_groups)

OUT.fit.CA_2F_groups = Acess_BD_withAFeAC %>% 
  group_by( fit_CA_2F_groups ) %>% 
  summarise(
    
    dC1_CA_2F = round(mean(PotDEMO2011_stand, na.rm = TRUE),2),
    dC2_CA_2F = round(mean(FA_AcessPot_scores, na.rm = TRUE),2),
    N_CA_2F = n()
  )


####
# 0 SAVES  ----
####

write_delim(Acess_BD_withAFeAC, paste(OUT.tables.path,"FREG_CAOP2018_withLocationDATA.txt", sep=""), quote_escape = "backslash")

save(Acess_BD_withAFeAC,
     file = paste(OUT.RData.path, "FREG_CAOP2018_Location_data.RData", sep="") )



#FREGCAOP2018_withDATA = CAOP2018_FREG_Descodifica.shp[,c("DICOFRE18","Shape" )]
#FREGCAOP2018_withDATA = left_join(FREGCAOP2018_withDATA, Acess_BD_withAFeAC, by = c("DICOFRE18" = "DICOFRE18") )
#st_write(FREGCAOP2018_withDATA, paste(ArcGIS_BRIDGE,"/FREG_CAOP2018_withLocationDATA.shp", sep=""), driver = "ESRI Shapefile" )
#colnames(FREGCAOP2018_withDATA)
