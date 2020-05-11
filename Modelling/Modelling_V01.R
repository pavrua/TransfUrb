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
#library("tidyselec")

library("psych")
library("nFactors")


####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA


setwd(paste(path, "02_RECURSOS/023_DSS_COMPONENTES/024_DEMOG_ECON/0241_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/Modelling", sep=""))

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "02_RECURSOS/020_DADOSGERAIS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")

OUT.tables.path = paste(path, "02_RECURSOS/023_DSS_COMPONENTES/024_DEMOG_ECON/0241_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")



####
# 0. LOAD PREPROCESSED DATA ----
####

CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")
CAOP2018_FREG_Descodifica = CAOP2018_FREG_Descodifica.shp %>% st_drop_geometry()

load( paste(path,"02_RECURSOS/023_DSS_COMPONENTES/024_DEMOG_ECON/0241_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/PreProcessing/OUT.TRANSFURBANAS.PREPROCESSING.V01.RData", sep="") )

BD_TRANSFURB = left_join(CAOP2018_FREG_Descodifica, OUT.ACESS_ALL_byFREG2018, by = c("DICOFRE18" = "DICOFRE18"))
BD_TRANSFURB = left_join(BD_TRANSFURB, OUT.POPCENSOS_2001e2011[,c(1,3,4,5,6)], by = c("DICOFRE18" = "DICOFRE18"))
BD_TRANSFURB = left_join(BD_TRANSFURB, OUT.POTDEMO2011byFREG2018, by = c("DICOFRE18" = "DICOFRE18"))
BD_TRANSFURB = left_join(BD_TRANSFURB, OUT.CAOP2018_COS2015_byCategN1_T1T2, by = c("DICOFRE18" = "DICOFRE18"))

BD_TRANSFURB$DensPOP2011_TerritArtif = BD_TRANSFURB$POP2011 / BD_TRANSFURB$AREACategN1_T1


####
# 1. ANALISE FATORIAL ----
####

# Factor analysis: https://www.statmethods.net/advstats/factor.html

dataFA = BD_TRANSFURB[,c("DensPOP2011",
                         "VarPOP0111",
                         "PotDEMO2011",
                         "PropAREACategN1_T1",
                         "PropAREACategN1_T2",
                         "DensPOP2011_TerritArtif"
                         )]

fit <- princomp(dataFA, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit) 

# Determine Number of Factors to Extract

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components
library(psych)
fit <- principal(mydata, nfactors=5, rotate="varimax")
fit # print results 






