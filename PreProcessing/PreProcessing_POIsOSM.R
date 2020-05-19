library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(xlsx)
library("sf")

####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/PreProcessing", sep=""))

OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")

ArcGIS_BRIDGE = paste(path, "01_BASESDEDADOS/SIG/Z_ArcGIS_BRIDGE", sep = "")

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")


CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")


#bounding box for Portugal https://www.openstreetmap.org/export#map=6/39.623/-8.459
m <- c(-9.602, 36.932, -6.152, 42.180)


####
# 1. GET POIs AMENITIES ----
####


amenities.list = read.xlsx( file = paste(OUT.tables.path, "POIsOSM.xlsx", sep=""), "amenities")

amenities.list.all.selec = filter(shops.list, shops.list$Selection == 1)

list.amenities.tags = available_tags("amenity")

#bounding box for Portugal https://www.openstreetmap.org/export#map=6/39.623/-8.459
m <- c(-9.602, 36.932, -6.152, 42.180)

#building the query
q.amenities.all <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(key = "amenity", value = amenities.list.all.selec$Value) 

amenities.list.all.getData = osmdata_sf(q.amenities.all)


amenities.all.dados.pontos = amenities.list.all.getData$osm_points
amenities.all.dados.pontos = amenities.all.dados.pontos[,c("osm_id", "name", "amenity", "amenity.type", "amenity_1", "amenity_2", "geometry" )]
amenities.all.dados.pontos_project = st_transform(amenities.all.dados.pontos, 3763)


amenities.all.dados.polyg = amenities.list.all.getData$osm_polygons
amenities.all.dados.polyg = amenities.all.dados.polyg[,c("osm_id", "name", "amenity", "amenity.type", "amenity_1", "amenity_2", "geometry" )]
amenities.all.dados.polyg_project = st_transform(amenities.all.dados.polyg, 3763)
amenities.all.dados.polyg_project_centroid = st_centroid(amenities.all.dados.polyg_project)
#st_crs(amenities.all.dados.polyg_project_centroid)

amenities.all.dados.multipolyg = amenities.list.all.getData$osm_multipolygons
amenities.all.dados.multipolyg = amenities.all.dados.multipolyg[,c("osm_id", "name", "amenity",  "geometry" )]
amenities.all.dados.multipolyg_project = st_transform(amenities.all.dados.multipolyg, 3763)
amenities.all.dados.multipolyg_project_centroid = st_centroid(amenities.all.dados.multipolyg_project)
amenities.all.dados.multipolyg_project_centroid$amenity.type = NA
amenities.all.dados.multipolyg_project_centroid$amenity_1 = NA
amenities.all.dados.multipolyg_project_centroid$amenity_2 = NA

amenities.all.dados.sig = rbind(amenities.all.dados.pontos_project, amenities.all.dados.polyg_project_centroid)
amenities.all.dados.sig = rbind(amenities.all.dados.sig, amenities.all.dados.multipolyg_project_centroid)

Encoding(amenities.all.dados.sig$name) <- "UTF-8"

amenities.all.dados.sig.withoutNA = amenities.all.dados.sig[complete.cases(amenities.all.dados.sig$amenity),]

CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")

amenities.all.dados.sig.withoutNA.CAOP2018 = st_join(amenities.all.dados.sig.withoutNA, CAOP2018_FREG_Descodifica.shp )
amenities.all.dados.sig.withoutNA.CAOP2018 = amenities.all.dados.sig.withoutNA.CAOP2018[complete.cases(amenities.all.dados.sig.withoutNA.CAOP2018$MUNICOD),]


st_write(amenities.all.dados.sig.withoutNA.CAOP2018, paste(ArcGIS_BRIDGE,"/amenities.all.dados.sig.withoutNA.CAOP2018.shp", sep=""), driver = "ESRI Shapefile" )



####
# 2. GET POIs SHOP ----
####

shops.list = read.xlsx( file = paste(OUT.tables.path, "POIsOSM.xlsx", sep=""), "shops")


list.shops.tags = available_tags("shop")



 ####
 # * 2.1 GET POIs SHOP ESSENTIALS ---
 ####
# 
# shops.list.essentials.selec = filter(shops.list, shops.list$Selection_2 == 1)
# 
# 
# #building the query
# q.shops.essentials <- m %>% 
#   opq (timeout = 25*100) %>%
#   add_osm_feature(key = "shop", value = shops.list.essentials.selec$Value) 
# 
# shops.list.essentials.getData = osmdata_sf(q.shops.essentials)
# 
# 
# shops.essentials.dados.pontos = shops.list.essentials.getData$osm_points
# shops.essentials.dados.pontos = shops.essentials.dados.pontos[,c("osm_id", "name", "amenity", "shop", "geometry" )]
# shops.essentials.dados.pontos_project = st_transform(shops.essentials.dados.pontos, 3763)
# 
# 
# shops.essentials.dados.polyg = shops.list.essentials.getData$osm_polygons
# shops.essentials.dados.polyg = shops.essentials.dados.polyg[,c("osm_id", "name", "amenity", "shop", "geometry" )]
# shops.essentials.dados.polyg_project = st_transform(shops.essentials.dados.polyg, 3763)
# shops.essentials.dados.polyg_project_centroid = st_centroid(shops.essentials.dados.polyg_project)
# #st_crs(amenities.all.dados.polyg_project_centroid)
# 
# shops.essentials.dados.multipolyg = shops.list.essentials.getData$osm_multipolygons
# shops.essentials.dados.multipolyg = shops.essentials.dados.multipolyg[,c("osm_id", "name", "shop", "geometry" )]
# shops.essentials.dados.multipolyg_project = st_transform(shops.essentials.dados.multipolyg, 3763)
# shops.essentials.dados.multipolyg_project_centroid = st_centroid(shops.essentials.dados.multipolyg_project)
# shops.essentials.dados.multipolyg_project_centroid$amenity = NA
# 
# shops.essentials.dados.sig = rbind(shops.essentials.dados.pontos_project, shops.essentials.dados.polyg_project_centroid)
# shops.essentials.dados.sig = rbind(shops.essentials.dados.sig, shops.essentials.dados.multipolyg_project_centroid)
# 
# Encoding(shops.essentials.dados.sig$name) <- "UTF-8"
# 
# shops.essentials.dados.sig.withoutNA = shops.essentials.dados.sig[complete.cases(shops.essentials.dados.sig$shop),]
# 
# 
# shops.essentials.dados.sig.withoutNA.CAOP2018 = st_join(shops.essentials.dados.sig.withoutNA, CAOP2018_FREG_Descodifica.shp )
# shops.essentials.dados.sig.withoutNA.CAOP2018 = shops.essentials.dados.sig.withoutNA.CAOP2018[complete.cases(shops.essentials.dados.sig.withoutNA.CAOP2018$MUNICOD),]
# 
# 
# st_write(shops.essentials.dados.sig.withoutNA.CAOP2018, paste(ArcGIS_BRIDGE,"/shops.essentials.dados.sig.withoutNA.CAOP2018F.shp", sep=""), driver = "ESRI Shapefile" )
# 

####
# * 2.2 GET POIs SHOP ALL ----
####

shops.list.all.selec = filter(shops.list, shops.list$Selection == 1)


#building the query
q.shops.all <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(key = "shop", value = shops.list.all.selec$Value) 

shops.list.all.getData = osmdata_sf(q.shops.all)


shops.all.dados.pontos = shops.list.all.getData$osm_points
shops.all.dados.pontos = shops.all.dados.pontos[,c("osm_id", "name", "amenity", "shop", "geometry" )]
shops.all.dados.pontos_project = st_transform(shops.all.dados.pontos, 3763)


shops.all.dados.polyg = shops.list.all.getData$osm_polygons
shops.all.dados.polyg = shops.all.dados.polyg[,c("osm_id", "name", "amenity", "shop", "geometry" )]
shops.all.dados.polyg_project = st_transform(shops.all.dados.polyg, 3763)
shops.all.dados.polyg_project_centroid = st_centroid(shops.all.dados.polyg_project)
#st_crs(amenities.all.dados.polyg_project_centroid)

shops.all.dados.multipolyg = shops.list.all.getData$osm_multipolygons
shops.all.dados.multipolyg = shops.all.dados.multipolyg[,c("osm_id", "name", "shop", "geometry" )]
shops.all.dados.multipolyg_project = st_transform(shops.all.dados.multipolyg, 3763)
shops.all.dados.multipolyg_project_centroid = st_centroid(shops.all.dados.multipolyg_project)
shops.all.dados.multipolyg_project_centroid$amenity = NA

shops.all.dados.sig = rbind(shops.all.dados.pontos_project, shops.all.dados.polyg_project_centroid)
shops.all.dados.sig = rbind(shops.all.dados.sig, shops.all.dados.multipolyg_project_centroid)

Encoding(shops.all.dados.sig$name) <- "UTF-8"

shops.all.dados.sig.withoutNA = shops.all.dados.sig[complete.cases(shops.all.dados.sig$amenity),]


shops.all.dados.sig.withoutNA.CAOP2018 = st_join(shops.all.dados.sig.withoutNA, CAOP2018_FREG_Descodifica.shp )
shops.all.dados.sig.withoutNA.CAOP2018 = shops.all.dados.sig.withoutNA.CAOP2018[complete.cases(shops.all.dados.sig.withoutNA.CAOP2018$MUNICOD),]


st_write(shops.all.dados.sig.withoutNA.CAOP2018, paste(ArcGIS_BRIDGE,"/shops.all.dados.sig.withoutNA.CAOP2018.shp", sep=""), driver = "ESRI Shapefile" )


####
# 3. GET POIs LEISURE ----
####


leisure.list = read.xlsx( file = paste(OUT.tables.path, "POIsOSM.xlsx", sep=""), "leisure")

#available_features()
#list.leisure.tags = available_tags("leisure")




leisure.list.all.selec = filter(leisure.list, leisure.list$Selection == 1)


#building the query
q.leisure.all <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(key = "leisure", value = leisure.list.all.selec$Value) 

leisure.list.all.getData = osmdata_sf(q.leisure.all)


leisure.all.dados.pontos = leisure.list.all.getData$osm_points
leisure.all.dados.pontos = leisure.all.dados.pontos[,c("osm_id", "name", "leisure", "amenity", "geometry" )]
leisure.all.dados.pontos_project = st_transform(leisure.all.dados.pontos, 3763)


leisure.all.dados.polyg = leisure.list.all.getData$osm_polygons
leisure.all.dados.polyg = leisure.all.dados.polyg[,c("osm_id", "name", "leisure", "amenity",  "geometry" )]
leisure.all.dados.polyg_project = st_transform(leisure.all.dados.polyg, 3763)
leisure.all.dados.polyg_project_centroid = st_centroid(leisure.all.dados.polyg_project)
#st_crs(amenities.all.dados.polyg_project_centroid)

leisure.all.dados.multipolyg = leisure.list.all.getData$osm_multipolygons
leisure.all.dados.multipolyg = leisure.all.dados.multipolyg[,c("osm_id", "name", "leisure", "amenity",  "geometry" )]
leisure.all.dados.multipolyg_project = st_transform(leisure.all.dados.multipolyg, 3763)
leisure.all.dados.multipolyg_project_centroid = st_centroid(leisure.all.dados.multipolyg_project)


leisure.all.dados.sig = rbind(leisure.all.dados.pontos_project, leisure.all.dados.polyg_project_centroid)
leisure.all.dados.sig = rbind(leisure.all.dados.sig, leisure.all.dados.multipolyg_project_centroid)

Encoding(leisure.all.dados.sig$name) <- "UTF-8"

leisure.all.dados.sig.withoutNA = leisure.all.dados.sig[complete.cases(leisure.all.dados.sig$leisure),]


leisure.all.dados.sig.withoutNA.CAOP2018 = st_join(leisure.all.dados.sig.withoutNA, CAOP2018_FREG_Descodifica.shp )
leisure.all.dados.sig.withoutNA.CAOP2018 = leisure.all.dados.sig.withoutNA.CAOP2018[complete.cases(leisure.all.dados.sig.withoutNA.CAOP2018$MUNICOD),]


st_write(leisure.all.dados.sig.withoutNA.CAOP2018, paste(ArcGIS_BRIDGE,"/leisure.all.dados.sig.withoutNA.CAOP2018F.shp", sep=""), driver = "ESRI Shapefile" )





####
# 4. GET POIs TOURISM ----
####




tourism.list = read.xlsx( file = paste(OUT.tables.path, "POIsOSM.xlsx", sep=""), "tourism")

available_features()
list.tourism.tags = available_tags("tourism")


tourism.list.all.selec = filter(tourism.list, tourism.list$Selection_all == 1)


#building the query
q.tourism.all <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(key = "tourism", value = tourism.list.all.selec$Value) 

tourism.list.all.getData = osmdata_sf(q.tourism.all)


tourism.all.dados.pontos = tourism.list.all.getData$osm_points
tourism.all.dados.pontos = tourism.all.dados.pontos[,c("osm_id", "name", "tourism", "amenity", "amenity_1", "geometry" )]
tourism.all.dados.pontos_project = st_transform(tourism.all.dados.pontos, 3763)



tourism.all.dados.polyg = tourism.list.all.getData$osm_polygons
tourism.all.dados.polyg = tourism.all.dados.polyg[,c("osm_id", "name", "tourism", "amenity", "amenity_1", "geometry"  )]
tourism.all.dados.polyg_project = st_transform(tourism.all.dados.polyg, 3763)
tourism.all.dados.polyg_project_centroid = st_centroid(tourism.all.dados.polyg_project)
#st_crs(amenities.all.dados.polyg_project_centroid)

tourism.all.dados.multipolyg = tourism.list.all.getData$osm_multipolygons
tourism.all.dados.multipolyg = tourism.all.dados.multipolyg[,c("osm_id", "name", "tourism", "amenity", "geometry" )]
tourism.all.dados.multipolyg_project = st_transform(tourism.all.dados.multipolyg, 3763)
tourism.all.dados.multipolyg_project_centroid = st_centroid(tourism.all.dados.multipolyg_project)
tourism.all.dados.multipolyg$amenity_1 = NA

tourism.all.dados.sig = rbind(tourism.all.dados.pontos_project, tourism.all.dados.polyg_project_centroid)
tourism.all.dados.sig = rbind(tourism.all.dados.sig, tourism.all.dados.multipolyg_project_centroid)

Encoding(tourism.all.dados.sig$name) <- "UTF-8"

tourism.all.dados.sig.withoutNA = tourism.all.dados.sig[complete.cases(tourism.all.dados.sig$tourism),]


tourism.all.dados.sig.withoutNA.CAOP2018 = st_join(tourism.all.dados.sig.withoutNA, CAOP2018_FREG_Descodifica.shp )
tourism.all.dados.sig.withoutNA.CAOP2018 = tourism.all.dados.sig.withoutNA.CAOP2018[complete.cases(tourism.all.dados.sig.withoutNA.CAOP2018$MUNICOD),]


st_write(tourism.all.dados.sig.withoutNA.CAOP2018, paste(ArcGIS_BRIDGE,"/tourism.all.dados.sig.withoutNA.CAOP2018F.shp", sep=""), driver = "ESRI Shapefile" )



