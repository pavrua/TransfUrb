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

#OUT.ArcGISBRIDGE.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/ArcGIS_BRIDGE/", sep = "")
OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")



####
# 0. LOAD PREPROCESSED DATA ----
####

load( paste(path,"02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/PreProcessing/OUT.BD_TRANSFURB.RData", sep="") )

CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")
CAOP2018_FREG_Descodifica = CAOP2018_FREG_Descodifica.shp %>% st_drop_geometry()

data.in = BD_TRANSFURB




####
# 1. Ranking Centralidade ----
####

pop2011 = data.in[,c("DICOFRE18", "POP_C2011_Total")]

pop2011_sort <- pop2011[order(-pop2011$POP_C2011_Total),]

pop2011_sort$FregPopRank = seq(1,nrow(pop2011_sort))


plot(pop2011_sort$FregPopRank, pop2011_sort$POP_C2011_Total)
barplot(pop2011_sort$POP_C2011_Total) 

plot(pop2011_sort$FregPopRank, pop2011_sort$POP_C2011_Total)

fit.lm.global = lm(log(pop2011_sort$POP_C2011_Total) ~ log(pop2011_sort$FregPopRank) )
summary(fit.lm.global)

fit.lm.global.residuals = fit.lm.global$residuals





  
####
# 2. ANALISE FATORIAL ----
####

#### * 2.1 AF GLOBAL ####

# Factor analysis: https://www.statmethods.net/advstats/factor.html

dataFA = data.in[,c("POP_C2011_DensPop",
                         "POP_VarPop_C2011C2015",
                         "POP_VarPop_C2001C2011",
                         "PotDEMO2011",
                         "PropAREACategN1_T1",
                         "PropAREACategN1_T2",
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

# ev <- eigen(cor(dataFA)) # get eigenvalues
# ap <- parallel(subject=nrow(dataFA),var=ncol(dataFA),
#                rep=100,cent=.05)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# plotnScree(nS) 

# Varimax Rotated Principal Components
# retaining 5 components
library(psych)
fit.fa <- principal(dataFA, nfactors=3, rotate="varimax")
fit.fa # print results 


data.in = cbind(data.in, fit.fa$scores)


#### * 2.2 AF I1 - Urbanidade ####

dataI1 = data.in[,c("POP_C2011_DensPop",
                         "PropAREACategN1_T1",
                         "DensPOP2011_TerritArtif"
                      )]
fit.I1.explo <- princomp(dataI1, cor=TRUE)
summary(fit.I1.explo) # print variance accounted for
loadings(fit.I1.explo) # pc loadings

library(psych)
fit.I1 <- principal(dataI1, nfactors=1, rotate="varimax")
fit.I1 # print results 

fit.I1$communality

data.in = cbind(data.in, fit.I1$scores )
colnames(data.in)[ncol(data.in)] = "I1_Urbanidade"



#### * 2.3 AF I2 - Centralidade ####

dataI2 = data.in[,c("PotDEMO2011",
                         "ACESS_ALL_byFREG2018",
                         "DensPOP2011_TerritArtif"
                      )]

fit.I2.explo <- princomp(dataI2, cor=TRUE)
summary(fit.I2.explo) # print variance accounted for
loadings(fit.I2.explo) # pc loadings

library(psych)
fit.I2 <- principal(dataI2, nfactors=1, rotate="varimax")
fit.I2 # print results 

fit.I2$communality

data.in = cbind(data.in, fit.I2$scores )
colnames(data.in)[ncol(data.in)] = "I2_Centralidade"


#### * 2.4 AF I3 - DinamicaPop ####

dataI3 = data.in[,c("POP_VarPop_C2001C2011",
                         "POP_VarPop_C2011C2015"
                      )]

fit.I3.explo <- princomp(dataI3, cor=TRUE)
summary(fit.I3.explo) # print variance accounted for
loadings(fit.I3.explo) # pc loadings

library(psych)
fit.I3 <- principal(dataI3, nfactors=1, rotate="varimax")
fit.I3 # print results 

fit.I3$communality

data.in = cbind(data.in, fit.I3$scores )
colnames(data.in)[ncol(data.in)] = "I3_DinamicaPop"

####
# 3. SPATIAL ANALYSIS MORAN & LISA ----
####

library("spdep")

#### * 3.1 CREATE QUEEN FREG18 ####


#### ** 3.1.1 1st Order ####
#Copy shapefile

ShapeFREG18 = CAOP2018_FREG_Descodifica.shp

#Add data to polygons
ShapeFREG18.addData = merge(ShapeFREG18, data.in , by.x = "DICOFRE18", by.y = "DICOFRE18")



# create row-standardized Queens contiguity weights matrix
# Queen 1st order
ShapeFREG18.nb_q_1order <- poly2nb(ShapeFREG18.addData)  #queen's neighborhood
ShapeFREG18.nb_q_1order.w <- nb2listw(ShapeFREG18.nb_q_1order)


#### ** 3.1.2 2sd Order ####

# Queen 2sd order
ShapeFREG18.nb_q_2order.aux = nblag(ShapeFREG18.nb_q_1order,2)
ShapeFREG18.nb_q_2order = nblag_cumul(ShapeFREG18.nb_q_2order.aux)
ShapeFREG18.nb_q_2order.w <- nb2listw(ShapeFREG18.nb_q_2order)


#### * 3.2 SPATIAL MODEL 1 ####

#### * 3.2.1 - I1 Urbanidade ####

#calculare Moran's I (global)
SAM_I1.moran.nb_q_1order <- moran.test(ShapeFREG18.addData$I1_Urbanidade, ShapeFREG18.nb_q_1order.w)

#calculate the local moran's I
SAM_I1.LocM.nb_q_1order <- localmoran(ShapeFREG18.addData$I1_Urbanidade, ShapeFREG18.nb_q_1order.w) 
summary(SAM_I1.LocM.nb_q_1order)


#save LocalMoran I1
data.in = cbind(data.in, SAM_I1.LocM.nb_q_1order[,1] )
colnames(data.in)[ncol(data.in)] = "I1_LocalMoran"

I1_LocalMoran_sign = as.matrix(ifelse(SAM_I1.LocM.nb_q_1order[,5]<0.05, SAM_I1.LocM.nb_q_1order[,1],0) )
data.in = cbind(data.in, I1_LocalMoran_sign )

# manually make a moran plot standarize variables
#ShapeFREG18.addData$I1_Urbanidade.scale <- scale(ShapeFREG18.addData$I1_Urbanidade)  #save to a new column
#summary(ShapeFREG18.addData$I1_Urbanidade.scale)

# create a lagged variable
ShapeFREG18.addData$I1_Urbanidade_lag <- lag.listw(ShapeFREG18.nb_q_1order.w, ShapeFREG18.addData$I1_Urbanidade)
summary(ShapeFREG18.addData$I1_Urbanidade_lag)


ggplot(ShapeFREG18.addData, aes( y = I1_Urbanidade_lag, x = I1_Urbanidade ) ) + 
  geom_point() +
  geom_smooth(method=lm, color="red")+
  labs(title = "Scatterplot :: Moran I \n - I1_Urbanidade -",
       y = "Spatial Lag - I1_Urbanidade", 
       x = "I1_Urbanidade") +
  geom_vline(xintercept=0, color = "black") +
  geom_hline(yintercept=0, color = "black")




# identify the moran plot quadrant for each observation
ShapeFREG18.addData$I1_Urbanidade_QuadSig <- NA
ShapeFREG18.addData$I1_Urbanidade_QuadSig <- factor(ShapeFREG18.addData$I1_Urbanidade_QuadSig, levels = c(1,2,3,4,5))
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade >= 0 & ShapeFREG18.addData$I1_Urbanidade_lag >= 0) & (SAM_I1.LocM.nb_q_1order[, 5] <= 0.05), "I1_Urbanidade_QuadSig"] <- 1
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade <= 0 & ShapeFREG18.addData$I1_Urbanidade_lag <= 0) & (SAM_I1.LocM.nb_q_1order[, 5] <= 0.05), "I1_Urbanidade_QuadSig"] <- 2
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade >= 0 & ShapeFREG18.addData$I1_Urbanidade_lag <= 0) & (SAM_I1.LocM.nb_q_1order[, 5] <= 0.05), "I1_Urbanidade_QuadSig"] <- 3
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade >= 0 & ShapeFREG18.addData$I1_Urbanidade_lag <= 0) & (SAM_I1.LocM.nb_q_1order[, 5] <= 0.05), "I1_Urbanidade_QuadSig"] <- 4
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade <= 0 & ShapeFREG18.addData$I1_Urbanidade_lag >= 0) & (SAM_I1.LocM.nb_q_1order[, 5] <= 0.05), "I1_Urbanidade_QuadSig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
ShapeFREG18.addData$I1_Urbanidade_QuadSig[is.na(ShapeFREG18.addData$I1_Urbanidade_QuadSig)] = 5

labels = c("High-High", "Low-Low", "High-Low", "Low-High", "No Significant")
col_list = c("red", "dark blue", "pink", "light blue", "grey80")

data.in = cbind(data.in, ShapeFREG18.addData$I1_Urbanidade_QuadSig )
colnames(data.in)[ncol(data.in)] = "I1_Urbanidade_QuadSig"

#ggplot( ) +
#  geom_sf( data = ShapeFREG18.addData, aes( fill = (I1_Urbanidade_QuadSig) )  ) +
#  #scale_color_manual( drop = TRUE, limits = levels(muni_pt_sf$HousePriceToIncome_QuadSig), values = col_list, guide = F) +
#  scale_fill_manual(drop = TRUE, limits = levels(ShapeFREG18.addData$I1_Urbanidade_QuadSig), values = col_list, labels = labels) +
#  guides(fill = guide_legend(title = "I1_Urbanidade \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )



#### * 3.2.1 - I1 Urbanidade :: neighs 2sd ####

#calculare Moran's I (global)
SAM_I1_neighs2.moran.nb_q_2order <- moran.test(ShapeFREG18.addData$I1_Urbanidade, ShapeFREG18.nb_q_2order.w)

#calculate the local moran's I
SAM_I1_neighs2.LocM.nb_q_2order <- localmoran(ShapeFREG18.addData$I1_Urbanidade, ShapeFREG18.nb_q_2order.w) 
summary(SAM_I1_neighs2.LocM.nb_q_2order)


#save LocalMoran I1
data.in = cbind(data.in, SAM_I1_neighs2.LocM.nb_q_2order[,1] )
colnames(data.in)[ncol(data.in)] = "I1_LocalMoran_2order"

I1_LocalMoran_2order_sign = as.matrix(ifelse(SAM_I1_neighs2.LocM.nb_q_2order[,5]<0.05, SAM_I1_neighs2.LocM.nb_q_2order[,1],0) )
data.in = cbind(data.in, I1_LocalMoran_2order_sign )


# create a lagged variable
ShapeFREG18.addData$I1_Urbanidade_lag2order <- lag.listw(ShapeFREG18.nb_q_2order.w, ShapeFREG18.addData$I1_Urbanidade)
summary(ShapeFREG18.addData$I1_Urbanidade_lag2order)


ggplot(ShapeFREG18.addData, aes( y = I1_Urbanidade_lag2order, x = I1_Urbanidade ) ) + 
  geom_point() +
  geom_smooth(method=lm, color="red")+
  labs(title = "Scatterplot :: Moran I \n - I1_Urbanidade -",
       y = "Spatial Lag - I1_Urbanidade_lag2order", 
       x = "I1_Urbanidade") +
  geom_vline(xintercept=0, color = "black") +
  geom_hline(yintercept=0, color = "black")




# identify the moran plot quadrant for each observation
ShapeFREG18.addData$I1_Urbanidade_2order_QuadSig <- NA
ShapeFREG18.addData$I1_Urbanidade_2order_QuadSig <- factor(ShapeFREG18.addData$I1_Urbanidade_QuadSig, levels = c(1,2,3,4,5))
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade >= 0 & ShapeFREG18.addData$I1_Urbanidade_lag2order >= 0) & (SAM_I1_neighs2.LocM.nb_q_2order[, 5] <= 0.05), "I1_Urbanidade_2order_QuadSig"] <- 1
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade <= 0 & ShapeFREG18.addData$I1_Urbanidade_lag2order <= 0) & (SAM_I1_neighs2.LocM.nb_q_2order[, 5] <= 0.05), "I1_Urbanidade_2order_QuadSig"] <- 2
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade >= 0 & ShapeFREG18.addData$I1_Urbanidade_lag2order <= 0) & (SAM_I1_neighs2.LocM.nb_q_2order[, 5] <= 0.05), "I1_Urbanidade_2order_QuadSig"] <- 3
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade >= 0 & ShapeFREG18.addData$I1_Urbanidade_lag2order <= 0) & (SAM_I1_neighs2.LocM.nb_q_2order[, 5] <= 0.05), "I1_Urbanidade_2order_QuadSig"] <- 4
ShapeFREG18.addData[(ShapeFREG18.addData$I1_Urbanidade <= 0 & ShapeFREG18.addData$I1_Urbanidade_lag2order >= 0) & (SAM_I1_neighs2.LocM.nb_q_2order[, 5] <= 0.05), "I1_Urbanidade_2order_QuadSig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
ShapeFREG18.addData$I1_Urbanidade_QuadSig[is.na(ShapeFREG18.addData$I1_Urbanidade_2order_QuadSig)] = 5

labels = c("High-High", "Low-Low", "High-Low", "Low-High", "No Significant")
col_list = c("red", "dark blue", "pink", "light blue", "grey80")

data.in = cbind(data.in, ShapeFREG18.addData$I1_Urbanidade_2order_QuadSig )
colnames(data.in)[ncol(data.in)] = "I1_Urbanidade_2order_QuadSig"

# ggplot( ) +
#  geom_sf( data = ShapeFREG18.addData, aes( fill = (I1_Urbanidade_2order_QuadSig) )  ) +
#  #scale_color_manual( drop = TRUE, limits = levels(muni_pt_sf$HousePriceToIncome_QuadSig), values = col_list, guide = F) +
#  scale_fill_manual(drop = TRUE, limits = levels(ShapeFREG18.addData$I1_Urbanidade_QuadSig), values = col_list, labels = labels) +
#  guides(fill = guide_legend(title = "I1_Urbanidade_2order \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )



#### * 3.2.2 - I2 Centralidade ####

#calculare Moran's I (global)
SAM_I2.moran.nb_q_1order <- moran.test(ShapeFREG18.addData$I2_Centralidade, ShapeFREG18.nb_q_1order.w)

#calculate the local moran's I
SAM_I2.LocM.nb_q_1order <- localmoran(ShapeFREG18.addData$I2_Centralidade, ShapeFREG18.nb_q_1order.w) 
summary(SAM_I2.LocM.nb_q_1order)


#save LocalMoran I1
data.in = cbind(data.in, SAM_I2.LocM.nb_q_1order[,1] )
colnames(data.in)[ncol(data.in)] = "I2_LocalMoran"

I2_LocalMoran_sign = as.matrix(ifelse(SAM_I2.LocM.nb_q_1order[,5]<0.05, SAM_I2.LocM.nb_q_1order[,1],0) )
data.in = cbind(data.in, I2_LocalMoran_sign )


# create a lagged variable
ShapeFREG18.addData$I2_Centralidade_lag <- lag.listw(ShapeFREG18.nb_q_1order.w, ShapeFREG18.addData$I2_Centralidade)
summary(ShapeFREG18.addData$I2_Centralidade_lag)


ggplot(ShapeFREG18.addData, aes( y = I2_Centralidade_lag, x = I2_Centralidade ) ) + 
  geom_point() +
  geom_smooth(method=lm, color="red")+
  labs(title = "Scatterplot :: Moran I \n - I2_Centralidade -",
       y = "Spatial Lag - I2_Centralidadee", 
       x = "I2_Centralidade") +
  geom_vline(xintercept=0, color = "black") +
  geom_hline(yintercept=0, color = "black")



# identify the moran plot quadrant for each observation
ShapeFREG18.addData$I2_Centralidade_QuadSig <- NA
ShapeFREG18.addData$I2_Centralidade_QuadSig <- factor(ShapeFREG18.addData$I2_Centralidade_QuadSig, levels = c(1,2,3,4,5))
ShapeFREG18.addData[(ShapeFREG18.addData$I2_Centralidade >= 0 & ShapeFREG18.addData$I2_Centralidade_lag >= 0) & (SAM_I2.LocM.nb_q_1order[, 5] <= 0.05), "I2_Centralidade_QuadSig"] <- 1
ShapeFREG18.addData[(ShapeFREG18.addData$I2_Centralidade <= 0 & ShapeFREG18.addData$I2_Centralidade_lag <= 0) & (SAM_I2.LocM.nb_q_1order[, 5] <= 0.05), "I2_Centralidade_QuadSig"] <- 2
ShapeFREG18.addData[(ShapeFREG18.addData$I2_Centralidade >= 0 & ShapeFREG18.addData$I2_Centralidade_lag <= 0) & (SAM_I2.LocM.nb_q_1order[, 5] <= 0.05), "I2_Centralidade_QuadSig"] <- 3
ShapeFREG18.addData[(ShapeFREG18.addData$I2_Centralidade >= 0 & ShapeFREG18.addData$I2_Centralidade_lag <= 0) & (SAM_I2.LocM.nb_q_1order[, 5] <= 0.05), "I2_Centralidade_QuadSig"] <- 4
ShapeFREG18.addData[(ShapeFREG18.addData$I2_Centralidade <= 0 & ShapeFREG18.addData$I2_Centralidade_lag >= 0) & (SAM_I2.LocM.nb_q_1order[, 5] <= 0.05), "I2_Centralidade_QuadSig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
ShapeFREG18.addData$I2_Centralidade_QuadSig[is.na(ShapeFREG18.addData$I2_Centralidade_QuadSig)] = 5

labels = c("High-High", "Low-Low", "High-Low", "Low-High", "No Significant")
col_list = c("red", "dark blue", "pink", "light blue", "grey80")

data.in = cbind(data.in, ShapeFREG18.addData$I2_Centralidade_QuadSig )
colnames(data.in)[ncol(data.in)] = "I2_Centralidade_QuadSig"

#ggplot( ) +
#  geom_sf( data = ShapeFREG18.addData, aes( fill = (I2_Centralidade_QuadSig) )  ) +
#  #scale_color_manual( drop = TRUE, limits = levels(muni_pt_sf$HousePriceToIncome_QuadSig), values = col_list, guide = F) +
#  scale_fill_manual(drop = TRUE, limits = levels(ShapeFREG18.addData$I2_Centralidade_QuadSig), values = col_list, labels = labels) +
#  guides(fill = guide_legend(title = "I2_Centralidade \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )




#### * 3.2.3 - I3 DinamicaPop ####

#calculare Moran's I (global)
SAM_I3.moran.nb_q_1order <- moran.test(ShapeFREG18.addData$I3_DinamicaPop, ShapeFREG18.nb_q_1order.w)

#calculate the local moran's I
SAM_I3.LocM.nb_q_1order <- localmoran(ShapeFREG18.addData$I3_DinamicaPop, ShapeFREG18.nb_q_1order.w) 
summary(SAM_I3.LocM.nb_q_1order)


#save LocalMoran I3
data.in = cbind(data.in, SAM_I3.LocM.nb_q_1order[,1] )
colnames(data.in)[ncol(data.in)] = "I3_LocalMoran"

I3_LocalMoran_sign = as.matrix(ifelse(SAM_I3.LocM.nb_q_1order[,5]<0.05, SAM_I3.LocM.nb_q_1order[,1],0) )
data.in = cbind(data.in, I3_LocalMoran_sign )


# create a lagged variable
ShapeFREG18.addData$I3_DinamicaPop_lag <- lag.listw(ShapeFREG18.nb_q_1order.w, ShapeFREG18.addData$I3_DinamicaPop)
summary(ShapeFREG18.addData$I3_DinamicaPop_lag)


ggplot(ShapeFREG18.addData, aes( y = I3_DinamicaPop_lag, x = I3_DinamicaPop ) ) + 
  geom_point() +
  geom_smooth(method=lm, color="red")+
  labs(title = "Scatterplot :: Moran I \n - I3_DinamicaPop -",
       y = "Spatial Lag - I3_DinamicaPop", 
       x = "I3_DinamicaPop") +
  geom_vline(xintercept=0, color = "black") +
  geom_hline(yintercept=0, color = "black")




# identify the moran plot quadrant for each observation
ShapeFREG18.addData$I3_DinamicaPop_QuadSig <- NA
ShapeFREG18.addData$I3_DinamicaPop_QuadSig <- factor(ShapeFREG18.addData$I3_DinamicaPop_QuadSig, levels = c(1,2,3,4,5))
ShapeFREG18.addData[(ShapeFREG18.addData$I3_DinamicaPop >= 0 & ShapeFREG18.addData$I3_DinamicaPop_lag >= 0) & (SAM_I3.LocM.nb_q_1order[, 5] <= 0.05), "I3_DinamicaPop_QuadSig"] <- 1
ShapeFREG18.addData[(ShapeFREG18.addData$I3_DinamicaPop <= 0 & ShapeFREG18.addData$I3_DinamicaPop_lag <= 0) & (SAM_I3.LocM.nb_q_1order[, 5] <= 0.05), "I3_DinamicaPop_QuadSig"] <- 2
ShapeFREG18.addData[(ShapeFREG18.addData$I3_DinamicaPop >= 0 & ShapeFREG18.addData$I3_DinamicaPop_lag <= 0) & (SAM_I3.LocM.nb_q_1order[, 5] <= 0.05), "I3_DinamicaPop_QuadSig"] <- 3
ShapeFREG18.addData[(ShapeFREG18.addData$I3_DinamicaPop >= 0 & ShapeFREG18.addData$I3_DinamicaPop_lag <= 0) & (SAM_I3.LocM.nb_q_1order[, 5] <= 0.05), "I3_DinamicaPop_QuadSig"] <- 4
ShapeFREG18.addData[(ShapeFREG18.addData$I3_DinamicaPop <= 0 & ShapeFREG18.addData$I3_DinamicaPop_lag >= 0) & (SAM_I3.LocM.nb_q_1order[, 5] <= 0.05), "I3_DinamicaPop_QuadSig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
ShapeFREG18.addData$I3_DinamicaPop_QuadSig[is.na(ShapeFREG18.addData$I3_DinamicaPop_QuadSig)] = 5

labels = c("High-High", "Low-Low", "High-Low", "Low-High", "No Significant")
col_list = c("red", "dark blue", "pink", "light blue", "grey80")

data.in = cbind(data.in, ShapeFREG18.addData$I3_DinamicaPop_QuadSig )
colnames(data.in)[ncol(data.in)] = "I3_DinamicaPop_QuadSig"

# ggplot( ) +
#  geom_sf( data = ShapeFREG18.addData, aes( fill = (I3_DinamicaPop_QuadSig) )  ) +
#  #scale_color_manual( drop = TRUE, limits = levels(muni_pt_sf$HousePriceToIncome_QuadSig), values = col_list, guide = F) +
#  scale_fill_manual(drop = TRUE, limits = levels(ShapeFREG18.addData$I3_DinamicaPop_QuadSig), values = col_list, labels = labels) +
#  guides(fill = guide_legend(title = "I3_DinamicaPop \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )



####
# 4. CLUSTERS ----
####

#### * 4.1 - Clusters deductive ####

data.in$Clusters_deductive_1 <- NA
data.in$Clusters_deductive_1 <- factor(data.in$Clusters_deductive_1, levels = c(1,2,3,4,5, 6, 7))

data.in[(data.in$RC3 <= -1.5 & data.in$RC1 >= 1.5 & data.in$RC2 >= 0) , "Clusters_deductive_1"] <- 1
data.in[(data.in$RC3 > -1.5 & data.in$RC3 <= 0 & data.in$RC1 < 1.5 & data.in$RC1 >= 0 & data.in$RC2 >= 0 ) , "Clusters_deductive_1"] <- 2
data.in[(data.in$RC3 > -1.5 & data.in$RC3 <= 0 & data.in$RC1 < 1.5 & data.in$RC1 >= 0 & data.in$RC2 < 0 ) , "Clusters_deductive_1"] <- 3
data.in[(data.in$RC3 >= 1.5 & data.in$RC1 < 1.5 & data.in$RC1 >= 0 & data.in$RC2 > 0 ) , "Clusters_deductive_1"] <- 4
data.in[(data.in$RC3 < 1.5 & data.in$RC3 > 0 & data.in$RC1 > -1.5 & data.in$RC1 <= 0 & data.in$RC2 < 0) , "Clusters_deductive_1"] <- 5
data.in[(data.in$RC3 < 1.5 & data.in$RC3 > 0 & data.in$RC1 > -1.5 & data.in$RC1 <= 0 & data.in$RC2 > 0) , "Clusters_deductive_1"] <- 6

data.in$Clusters_deductive_1[is.na(data.in$Clusters_deductive_1)] = 7


#### * 4.2 - Clusters AF Global ####

dataCA= data.in[,c("RC1","RC2","RC3")]

# Ward Hierarchical Clustering
d <- dist(dataCA, method = "euclidean") # distance matrix
fit.clusters <- hclust(d, method="ward.D")
plot(fit.clusters) # display dendogram
fit_clusters_groups <- cutree(fit.clusters, k=6) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.clusters, k=6, border="red")

data.in = cbind(data.in, fit_clusters_groups)


OUT.fit_clusters_groups = data.in %>% 
  group_by( fit_clusters_groups ) %>% 
  summarise(
    
    dRC1 = round(mean(RC1, na.rm = TRUE),2),
    dRC2 = round(mean(RC2, na.rm = TRUE),2),
    dRC3 = round(mean(RC3, na.rm = TRUE),2)
    
  )


#### ** 4.2.1 - Cluster RC1 ####

dataCA_RC1= data.in[,c("RC1")]

# Ward Hierarchical Clustering
d <- dist(dataCA_RC1, method = "euclidean") # distance matrix
fit.clusters.RC1 <- hclust(d, method="ward.D")
plot(fit.clusters) # display dendogram
RC1_fit_clusters_groups <- cutree(fit.clusters.RC1, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.clusters.RC1, k=4, border="red")


data.in = cbind(data.in, RC1_fit_clusters_groups)

#### ** 4.2.1 - Cluster RC3 ####

dataCA_RC3= data.in[,c("RC3")]

# Ward Hierarchical Clustering
d <- dist(dataCA_RC3, method = "euclidean") # distance matrix
fit.clusters.RC3 <- hclust(d, method="ward.D")
plot(fit.clusters) # display dendogram
RC3_fit_clusters_groups <- cutree(fit.clusters.RC3, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.clusters.RC3, k=4, border="red")


data.in = cbind(data.in, RC3_fit_clusters_groups)



#### * 4.3 - Clusters Dim ####

dataCDim= data.in[,c("I1_Urbanidade","I2_Centralidade","I3_DinamicaPop",
                   "I1_LocalMoran","I2_LocalMoran", "I3_LocalMoran" 
                   )]

# Ward Hierarchical Clustering
d <- dist(dataCDim, method = "euclidean") # distance matrix
fit.clustersDim <- hclust(d, method="ward.D")
plot(fit.clustersDim) # display dendogram
fit.clustersDim_groups <- cutree(fit.clustersDim, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit.clustersDim, k=5, border="red")


data.in = cbind(data.in, fit.clustersDim_groups)

OUT.fit.clustersDim_groups = data.in %>% 
  group_by( fit.clustersDim_groups ) %>% 
  summarise(
    
    dC1 = round(mean(I1_Urbanidade, na.rm = TRUE),2),
    dC2 = round(mean(I2_Centralidade, na.rm = TRUE),2),
    dC3 = round(mean(I3_DinamicaPop, na.rm = TRUE),2),
    dC4 = round(mean(I1_LocalMoran, na.rm = TRUE),2),
    dC5 = round(mean(I2_LocalMoran, na.rm = TRUE),2),
    dC6 = round(mean(I3_LocalMoran, na.rm = TRUE),2)
  )

ShapeFREG18.addData = merge(ShapeFREG18, data.in[,c("DICOFRE18" ,"fit.clustersDim_groups" )] , by.x = "DICOFRE18", by.y = "DICOFRE18")


ggplot( ) +
  geom_sf( data = ShapeFREG18.addData, aes( fill = (as.character(fit.clustersDim_groups)) )  )  +
  #scale_color_manual( drop = TRUE, limits = levels(muni_pt_sf$HousePriceToIncome_QuadSig), values = col_list, guide = F) +
  #scale_fill_manual(drop = TRUE, limits = levels(ShapeFREG18.addData$I3_DinamicaPop_QuadSig), values = col_list, labels = labels) +
  #guides(fill = guide_legend(title = "I3_DinamicaPop \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )
  scale_fill_brewer(as.character(fit.clustersDim_groups))


####
# 0. SAVE GERAL ----
####

# * 0. Save shapefile ----

selectCols = c("DICOFRE18",
               "POP_C2011_DensPop",
               "POP_VarPop_C2011C2015",
               "PotDEMO2011",
               "PropAREACategN1_T1",
               "PropAREACategN1_T2",
               "DensPOP2011_TerritArtif",
               "ACESS_ALL_byFREG2018",
               "RC1", "RC2","RC3",
               "Clusters_deductive_1","fit_clusters_groups")

CAOP2018_FREG_Descodifica.shp.out = CAOP2018_FREG_Descodifica.shp
CAOP2018_FREG_Descodifica.shp.out = left_join(CAOP2018_FREG_Descodifica.shp.out, data.in[,selectCols], by = c("DICOFRE18" = "DICOFRE18"))


st_write(CAOP2018_FREG_Descodifica.shp.out, dsn = paste(OUT.ArcGISBRIDGE.path, "TRANSFURBANAS_DADOSMODELLING_V1211.shp", sep = ""),  driver="ESRI Shapefile" )





write_delim(data.in, paste(OUT.tables.path,"data_in.txt", sep=""), quote_escape = "backslash")

