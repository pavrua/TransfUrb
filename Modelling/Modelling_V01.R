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

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/Modelling", sep=""))

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")

OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")



####
# 0. LOAD PREPROCESSED DATA ----
####

load( paste(path,"02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/PreProcessing/OUT.BD_TRANSFURB.RData", sep="") )

  
####
# 1. ANALISE FATORIAL ----
####

# Factor analysis: https://www.statmethods.net/advstats/factor.html

dataFA = BD_TRANSFURB[,c("POP_C2011_DensPop",
                         "POP_VarPop_C2011C2015",
                         "PotDEMO2011",
                         "PropAREACategN1_T1",
                         #"PropAREACategN1_T2",
                         "DensPOP2011_TerritArtif",
                         "ACESS_ALL_byFREG2018"
                         )]

fit <- princomp(dataFA, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
#plot(fit,type="lines") # scree plot
#fit$scores # the principal components
#biplot(fit) 

# Determine Number of Factors to Extract

ev <- eigen(cor(dataFA)) # get eigenvalues
ap <- parallel(subject=nrow(dataFA),var=ncol(dataFA),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components
library(psych)
fit <- principal(dataFA, nfactors=3, rotate="varimax")
fit # print results 






