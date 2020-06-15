####
# | Criado em: 2020-06-15 |
# PROCESSOS TRANSFORMACAO URBBANA
# MODELACAO «TRANSFORMACOES URBANANAS - INTERACOES ESPACIAIS» Versao 01 | 2020-06-15   ----
####

####
# 0. LIBRARIES ----
####

library("tidyverse")

library("xlsx")
library("openxlsx")

library("psych")
library("nFactors")

library("ggplot2")

library("spdep")
library("gstat")

library("sf")
library("rgdal")
library("maptools")

library("RColorBrewer")
library("classInt")

library("stringr")
#library("magrittr")



####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/Modelling_Spatial", sep=""))

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")
sourceSIG_GEODATA_VARIOS_gdb = paste(path, "01_BASESDEDADOS/SIG/GEODATA_VARIOS.gdb", sep = "")

# pathDADOSBASE_DRIVITUP_POPPROJECCOES =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GE5anos_PROJECCOES/", sep ="") 
# pathDADOSBASE_DRIVITUP_INE_ESTIMATIVAS =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GE5anos__INE_ESTIMATIVAS/", sep ="") 
# pathDADOSBASE_DRIVITUP_POPCENSOS =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GE5anos_CENSOS_91_01_11/", sep ="") 
# pathDADOSBASE_DRIVITUP_GEODESCODIFICA =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/CAOP_DESCODIFICA/", sep ="") 
# pathDADOSBASE_DRIVITUP_BGRI2011 =  paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/CENSOS2011_BGRI/", sep ="") 
# pathDADOSBASE_DRIVITUP_INEPEDIDO1 = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/INE_PEDIDO_1/", sep ="") 
# pathDADOSBASE_DRIVITUP_INEPEDIDO2 = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/INE_PEDIDO_2/", sep ="") 
# pathDADOSBASE_DRIVITUP_TABELASVARIAS = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/INE_TABELAS_VARIAS/", sep ="") 
# pathDADOSBASE_DRIVITUP_POPGLOBAL = paste(dir, "Universidade de Aveiro/OP_DRIVIT-UP - Documentos/02_RECURSOS/020_DADOSGERAIS/TABELAS/POPULACAO_GLOBAL/", sep ="") 



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

#DADOS TU CLASSIFICATION V8 
#FICHEIROS: db.COS_COMBINE, OUT.CA_COS_COMBINE_F0_Wardgroups
load( paste(OUT.RData.path,"FREG_CAOP2018_CLASSIFICATION_V8.RData", sep="") )


####
# 2. SPATIAL ANALYSIS  ####
####

# 2.0.1 GET AND PREPARE SPATIAL DATA ####

#muni_pt <- readOGR(dsn = pathDADOSBASE_SIG , layer = "CAOP2018_MUNI", encoding = "UTF-8", use_iconv = TRUE)

#Read shapefile of Municipalities in PT - using library sf
shape_freg18_pt <- st_read(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb , layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")



#Add data to polygons
shape_freg18_pt.addData = left_join(shape_freg18_pt, db.ALL, by  = c("DICOFRE18","DICOFRE18"))



# create row-standardized Queens contiguity weights matrix

# Queen 1st order
shape_freg18_pt.nb_q_1order <- poly2nb(shape_freg18_pt.addData)  #queen's neighborhood
shape_freg18_pt.nb_q_1order.w <- nb2listw(shape_freg18_pt.nb_q_1order)






####
# 2.1.1 SPAT. ANAL. POP_CHANGE #### ####
####


ggplot(shape_freg18_pt.addData, aes( x = "" , y = POPCHANGE_scale) ) + 
  geom_boxplot() +
  labs(title = "Box-Plot \n ---",
       y = "POPCHANGE_scale"
  )


ggplot(shape_freg18_pt.addData, aes(x = POPCHANGE_scale) ) + 
  geom_histogram(binwidth=0.05, color="black", fill="white") +
  labs(title = "Histogram Plot \n ---",
       y = "POPCHANGE_scale"
  )






#### QUEEN 1stORDER ###

#calculare Moran's I (global)
sd_PopChange.moran.nb_q_1order <- moran.test(shape_freg18_pt.addData$POPCHANGE_scale, shape_freg18_pt.nb_q_1order.w)

#calculate the local moran's I
sd_PopChange.LocM.nb_q_1order <- localmoran(as.vector(shape_freg18_pt.addData$POPCHANGE_scale), shape_freg18_pt.nb_q_1order.w, zero.policy=T) 
summary(sd_PopChange.LocM.nb_q_1order)

# # manually make a moran plot standarize variables
# shape_freg18_pt.addData$HousePriceToIncomeFinal.scale <- scale(shape_freg18_pt.addData$HousePriceToIncomeFinal)  #save to a new column
# summary(shape_freg18_pt.addData$HousePriceToIncomeFinal.scale)

# create a lagged variable
shape_freg18_pt.addData$POPCHANGE_scale_lag <- lag.listw(shape_freg18_pt.nb_q_1order.w, as.vector(shape_freg18_pt.addData$POPCHANGE_scale))
summary(shape_freg18_pt.addData$POPCHANGE_scale_lag)


ggplot(shape_freg18_pt.addData, aes( y = POPCHANGE_scale_lag, x = POPCHANGE_scale ) ) + 
  geom_point() +
  geom_smooth(method=lm, color="red")+
  labs(title = "Scatterplot :: Moran I \n - House Price To Income -",
       y = "Spatial Lag - POPCHANGE_scale", 
       x = "POPCHANGE_scale") +
  geom_vline(xintercept=0, color = "black") +
  geom_hline(yintercept=0, color = "black")




# identify the moran plot quadrant for each observation
shape_freg18_pt.addData$POPCHANGE_QuadSig <- NA
shape_freg18_pt.addData$POPCHANGE_QuadSig <- factor(shape_freg18_pt.addData$POPCHANGE_QuadSig, levels = c(1,2,3,4,5))
shape_freg18_pt.addData[(shape_freg18_pt.addData$POPCHANGE_scale >= 0 & shape_freg18_pt.addData$POPCHANGE_scale_lag >= 0) & (SA211.LocM.nb_q_1order[, 5] <= 0.05), "POPCHANGE_QuadSig"] <- 1
shape_freg18_pt.addData[(shape_freg18_pt.addData$POPCHANGE_scale <= 0 & shape_freg18_pt.addData$POPCHANGE_scale_lag <= 0) & (SA211.LocM.nb_q_1order[, 5] <= 0.05), "POPCHANGE_QuadSig"] <- 2
shape_freg18_pt.addData[(shape_freg18_pt.addData$POPCHANGE_scale >= 0 & shape_freg18_pt.addData$POPCHANGE_scale_lag <= 0) & (SA211.LocM.nb_q_1order[, 5] <= 0.05), "POPCHANGE_QuadSig"] <- 3
shape_freg18_pt.addData[(shape_freg18_pt.addData$POPCHANGE_scale >= 0 & shape_freg18_pt.addData$POPCHANGE_scale_lag <= 0) & (SA211.LocM.nb_q_1order[, 5] <= 0.05), "POPCHANGE_QuadSig"] <- 4
shape_freg18_pt.addData[(shape_freg18_pt.addData$POPCHANGE_scale <= 0 & shape_freg18_pt.addData$POPCHANGE_scale_lag >= 0) & (SA211.LocM.nb_q_1order[, 5] <= 0.05), "POPCHANGE_QuadSig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
shape_freg18_pt.addData$POPCHANGE_QuadSig[is.na(shape_freg18_pt.addData$POPCHANGE_QuadSig)] = 5

labels = c("High-High", "Low-Low", "High-Low", "Low-High", "No Significant")
col_list = c("red", "dark blue", "pink", "light blue", "grey80")


ggplot( ) +
  geom_sf( data = shape_freg18_pt.addData, aes( fill = (POPCHANGE_QuadSig) )  ) +
  scale_fill_manual(drop = TRUE, limits = levels(shape_freg18_pt.addData$POPCHANGE_QuadSig), values = col_list, labels = labels) +
  guides(fill = guide_legend(title = "POPCHANGE_QuadSig \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )




####
# 6 SPATIAL ANALYSIS BI-VARIATE   ####
####


####
# 6.0 Exploratory - Descriptive analysis  ####
####


cor(shape_freg18_pt.addData$FA_LOCALITY, shape_freg18_pt.addData$POPCHANGE_scale, use = "complete.obs")
cor.test(shape_freg18_pt.addData$FA_LOCALITY, shape_freg18_pt.addData$POPCHANGE_scale, use = "complete.obs")

ggplot(shape_freg18_pt.addData, aes( y = FA_LOCALITY, x = POPCHANGE_scale   ) ) + 
  geom_point() +
  geom_smooth(method=lm, color="red") +
  labs(title = "Scatter Plot \n ---",
       y = "FA_LOCALITY ", 
       x = "POPCHANGE_scale")









# Example: Variables to use in the correlation: white and black population in each census track
#x <- map$income
#y <- map$diffaccess


# 6.1.0 Auxiliar functions ####
# Programming some auxiliar functions for bi-variate spatial autocorrelation

# Bivariate Moran's I
moran_I_bivariate <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  
  xp <- scale(x)[, 1]
  yp <- scale(y)[, 1]
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  
  list(global = global, local  = as.numeric(local))
}


# Permutations for the Bivariate Moran's I
simula_moran_bivariate <- function(x, y = NULL, W, nsims = 2000){
  
  if(is.null(y)) y = x
  
  n   = nrow(W)
  IDs = 1:n
  
  xp <- scale(x)[, 1]
  W[which(is.na(W))] <- 0
  
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)
  
  list(global_sims = global_sims,
       local_sims  = local_sims)
}




lw <- nb2listw(shape_freg18_pt.nb_q_1order, style = "B")
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W)
Wrs  <- W/rowSums(W)
Wrs[which(is.na(Wrs))] <- 0



####
# 6.2.1 LISA BI-VARIATE FA_Locality vs PopChange  ####
####

x = shape_freg18_pt.addData$FA_LOCALITY
y = shape_freg18_pt.addData$POPCHANGE_scale

# Calculating the index and its simulated distribution
# for global and local values

m <- moran_I_bivariate(x, y, W)

# Global Moral
global_moran <- m[[1]][1]


# Local values
m_i <- m[[2]] 

# local simulations
local_sims <- simula_moran_bivariate(x, y, W)$local_sims


# global pseudo p-value  
# get all simulated global moran
global_sims <- simula_moran_bivariate(x, y, W)$global_sims

# Proportion of simulated global values taht are higher (in absolute terms) than the actual index 
moran_pvalue <- sum(abs(global_sims) > abs( global_moran )) / length(global_sims)
#> 0


# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig       <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )




# Preparing for plotting


# Convert shape file into sf object
#map_sf     <- st_as_sf(map)
#map_sf$sig <- sig
shape_freg18_pt.addData$Locality_PopChang_BiVariateQuadSig <- NA

# Identifying the LISA clusters
xp <- scale(x)[,1]
yp <- scale(y)[,1]


patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")

patterns[shape_freg18_pt.addData$Locality_PopChang_BiVariateQuadSig==0] <- "Not significant"
shape_freg18_pt.addData$Locality_PopChang_BiVariateQuadSig.patterns <- patterns


# Rename LISA clusters



col_list_finalBi = c("red","pink" , "light blue", "dark blue", "grey80")

### PLOT

ggplot( ) +
  geom_sf( data = shape_freg18_pt.addData, aes( fill = (Locality_PopChang_BiVariateQuadSig.patterns) )  ) +
  #scale_color_manual( drop = TRUE, limits = levels(muni_pt_sf$HousePriceToIncome_QuadSig), values = col_list, guide = F) +
  scale_fill_manual(drop = TRUE, limits = levels(shape_freg18_pt.addData$Locality_PopChang_BiVariateQuadSig.patterns), values = col_list_finalBi ) +
  guides(fill = guide_legend(title = "Locality - PopChang \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )
# + theme_minimal()





####
# 6.2 LISA BI-VARIATE FA_Location vs PopChange  ####
####

x = shape_freg18_pt.addData$FA_LOCATION
y = shape_freg18_pt.addData$POPCHANGE_scale

# Calculating the index and its simulated distribution
# for global and local values

m <- moran_I_bivariate(x, y, W)

# Global Moral
global_moran <- m[[1]][1]


# Local values
m_i <- m[[2]] 

# local simulations
local_sims <- simula_moran_bivariate(x, y, W)$local_sims


# global pseudo p-value  
# get all simulated global moran
global_sims <- simula_moran_bivariate(x, y, W)$global_sims

# Proportion of simulated global values taht are higher (in absolute terms) than the actual index 
moran_pvalue <- sum(abs(global_sims) > abs( global_moran )) / length(global_sims)
#> 0


# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig       <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )




# Preparing for plotting


# Convert shape file into sf object
#map_sf     <- st_as_sf(map)
#map_sf$sig <- sig
shape_freg18_pt.addData$Location_PopChang_BiVariateQuadSig <- NA

# Identifying the LISA clusters
xp <- scale(x)[,1]
yp <- scale(y)[,1]


patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")

patterns[shape_freg18_pt.addData$Location_PopChang_BiVariateQuadSig==0] <- "Not significant"
shape_freg18_pt.addData$Location_PopChang_BiVariateQuadSig.patterns <- patterns


# Rename LISA clusters



col_list_finalBi = c("red","pink" , "light blue", "dark blue", "grey80")

### PLOT

ggplot( ) +
  geom_sf( data = shape_freg18_pt.addData, aes( fill = (Location_PopChang_BiVariateQuadSig.patterns) )  ) +
  #scale_color_manual( drop = TRUE, limits = levels(muni_pt_sf$HousePriceToIncome_QuadSig), values = col_list, guide = F) +
  scale_fill_manual(drop = TRUE, limits = levels(shape_freg18_pt.addData$Location_PopChang_BiVariateQuadSig.patterns), values = col_list_finalBi ) +
  guides(fill = guide_legend(title = "Location - PopChang \nLISA :: QUEEN NEIGHBRS 1st Order", title.position = "top") )
# + theme_minimal()




####
# 0 SAVES  ----
####


db.SP_Data = shape_freg18_pt.addData %>% st_drop_geometry()

write_delim(db.SP_Data, paste(OUT.tables.path,"FREG_CAOP2018_SpatialDependence.txt", sep=""), quote_escape = "backslash")
# 
# save(BD_TU_GLOBAL, OUT.TRANSF_URB,
#      file = paste(OUT.RData.path, "FREG_CAOP2018_TRANSFURB_GLOBAL_data.RData", sep="") )
# 




