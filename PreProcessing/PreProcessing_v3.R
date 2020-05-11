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



####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/PreProcessing", sep=""))


OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")


sourceTABLE_demografia = paste(path, "01_BASESDEDADOS/TABELAS/DEMOGRAFIA/POPULACAO_GE5anos_CENSOS_91_01_11/", sep="")

sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")
sourceSIG_GEODATA_VARIOS_gdb = paste(path, "01_BASESDEDADOS/SIG/GEODATA_VARIOS.gdb", sep = "")
sourceSIG_POIs_gdb = paste(path, "01_BASESDEDADOS/SIG/OSM_POIs_20200211/OSM_pois.gdb", sep = "")
sourceSIG_COS_gdb = paste(path, "01_BASESDEDADOS/SIG/COS/COS.gdb", sep = "")






####
# 1. DEMOGRAFIA ----
####

####
# * 1.1 PROCESS INE POP CENSOS 1991 - 2001 - 2011  ----
####



#CENSOS1991_POP = read.delim(file = paste(sourceTABLE_demografia, "CENSOS1991_POPULACAO.txt",sep=""), header = TRUE)
CENSOS1991_POP =  read_delim(file = paste(sourceTABLE_demografia, "C1991_BGRI_GE5_Ori.txt",sep = ""), delim = "\t", 
                             col_types = cols_only(SUBSEC = col_character(),
                                                   SEXO_DSG = col_character(),
                                                   Total = col_double(),
                                                   GE_0_4 = col_double(),
                                                   GE_5_9 = col_double(),
                                                   GE_10_14 = col_double(),
                                                   GE_15_19 = col_double(),
                                                   GE_20_24 = col_double(),
                                                   GE_25_29 = col_double(),
                                                   GE_30_34 = col_double(),
                                                   GE_35_39 = col_double(),
                                                   GE_40_44 = col_double(),
                                                   GE_45_49 = col_double(),
                                                   GE_50_54 = col_double(),
                                                   GE_55_59 = col_double(),
                                                   GE_60_64 = col_double(),
                                                   GE_65_69 = col_double(),
                                                   GE_70_74 = col_double(),
                                                   GE_75_79 = col_double(),
                                                   GE_80_84 = col_double(),
                                                   GE_85_89 = col_double(),
                                                   GE_90_94 = col_double(),
                                                   GE_95_99 = col_double(),
                                                   GE_100e = col_double()
                             )
)



CENSOS1991_POP = subset(CENSOS1991_POP, SEXO_DSG == "HM")

CENSOS1991_POP$C91_GETotal = CENSOS1991_POP$Total
CENSOS1991_POP$C91_GEate20 = CENSOS1991_POP$GE_0_4 + CENSOS1991_POP$GE_5_9 + CENSOS1991_POP$GE_10_14 + CENSOS1991_POP$GE_15_19
CENSOS1991_POP$C91_GE2065 = CENSOS1991_POP$GE_20_24 + CENSOS1991_POP$GE_25_29 + CENSOS1991_POP$GE_30_34 + CENSOS1991_POP$GE_35_39 +
  CENSOS1991_POP$GE_40_44 + CENSOS1991_POP$GE_45_49 + CENSOS1991_POP$GE_50_54 + CENSOS1991_POP$GE_55_59 + CENSOS1991_POP$GE_60_64
CENSOS1991_POP$C91_GEmais65 = CENSOS1991_POP$GE_65_69 + CENSOS1991_POP$GE_70_74 + CENSOS1991_POP$GE_75_79 + CENSOS1991_POP$GE_80_84 +
  CENSOS1991_POP$GE_85_89 + CENSOS1991_POP$GE_90_94 + CENSOS1991_POP$GE_95_99 + CENSOS1991_POP$GE_100e 


#CENSOS2001_POP = read.delim(file = paste(sourceTABLE_demografia, "CENSOS2001_POPULACAO.txt",sep=""), header = TRUE)
CENSOS2001_POP = read_delim(file = paste(sourceTABLE_demografia, "C2001_BGRI_GE5_Ori.txt",sep = ""), delim = "\t", 
                            col_types = cols_only(SUBSEC_EU02 = col_character(),
                                                  SEXO = col_character(),
                                                  Total = col_double(),
                                                  GE_0_4 = col_double(),
                                                  GE_5_9 = col_double(),
                                                  GE_10_14 = col_double(),
                                                  GE_15_19 = col_double(),
                                                  GE_20_24 = col_double(),
                                                  GE_25_29 = col_double(),
                                                  GE_30_34 = col_double(),
                                                  GE_35_39 = col_double(),
                                                  GE_40_44 = col_double(),
                                                  GE_45_49 = col_double(),
                                                  GE_50_54 = col_double(),
                                                  GE_55_59 = col_double(),
                                                  GE_60_64 = col_double(),
                                                  GE_65_69 = col_double(),
                                                  GE_70_74 = col_double(),
                                                  GE_75_79 = col_double(),
                                                  GE_80_84 = col_double(),
                                                  GE_85_89 = col_double(),
                                                  GE_90_94 = col_double(),
                                                  GE_95_99 = col_double(),
                                                  GE_100e = col_double()
                            )
)
CENSOS2001_POP = subset(CENSOS2001_POP, SEXO == "HM")


CENSOS2001_POP$C01_GETotal = CENSOS2001_POP$Total
CENSOS2001_POP$C01_GEate20 = CENSOS2001_POP$GE_0_4 + CENSOS2001_POP$GE_5_9 + CENSOS2001_POP$GE_10_14 + CENSOS2001_POP$GE_15_19
CENSOS2001_POP$C01_GE2065 = CENSOS2001_POP$GE_20_24 + CENSOS2001_POP$GE_25_29 + CENSOS2001_POP$GE_30_34 + CENSOS2001_POP$GE_35_39 +
  CENSOS2001_POP$GE_40_44 + CENSOS2001_POP$GE_45_49 + CENSOS2001_POP$GE_50_54 + CENSOS2001_POP$GE_55_59 + CENSOS2001_POP$GE_60_64
CENSOS2001_POP$C01_GEmais65 = CENSOS2001_POP$GE_65_69 + CENSOS2001_POP$GE_70_74 + CENSOS2001_POP$GE_75_79 + CENSOS2001_POP$GE_80_84 +
  CENSOS2001_POP$GE_85_89 + CENSOS2001_POP$GE_90_94 + CENSOS2001_POP$GE_95_99 + CENSOS2001_POP$GE_100e


#CENSOS2011_POP = read.delim(file = paste(sourceTABLE_demografia, "CENSOS2011_POPULACAO.txt",sep=""), header = TRUE, sep = "\t")
CENSOS2011_POP = read_delim(file = paste(sourceTABLE_demografia, "C2011_BGRI_GE5_Ori.txt",sep = ""), delim = "\t", 
                            col_types = cols_only(SUBSECCAO = col_character(),
                                                  SEXO = col_character(),
                                                  Total = col_double(),
                                                  GE_0_4 = col_double(),
                                                  GE_5_9 = col_double(),
                                                  GE_10_14 = col_double(),
                                                  GE_15_19 = col_double(),
                                                  GE_20_24 = col_double(),
                                                  GE_25_29 = col_double(),
                                                  GE_30_34 = col_double(),
                                                  GE_35_39 = col_double(),
                                                  GE_40_44 = col_double(),
                                                  GE_45_49 = col_double(),
                                                  GE_50_54 = col_double(),
                                                  GE_55_59 = col_double(),
                                                  GE_60_64 = col_double(),
                                                  GE_65_69 = col_double(),
                                                  GE_70_74 = col_double(),
                                                  GE_75_79 = col_double(),
                                                  GE_80_84 = col_double(),
                                                  GE_85_89 = col_double(),
                                                  GE_90_94 = col_double(),
                                                  GE_95_99 = col_double(),
                                                  GE_100e = col_double()
                            )
)

CENSOS2011_POP = subset(CENSOS2011_POP, SEXO == "HM")

CENSOS2011_POP$C11_GETotal = CENSOS2011_POP$Total
CENSOS2011_POP$C11_GEate20 = CENSOS2011_POP$GE_0_4 + CENSOS2011_POP$GE_5_9 + CENSOS2011_POP$GE_10_14 + CENSOS2011_POP$GE_15_19
CENSOS2011_POP$C11_GE2065 = CENSOS2011_POP$GE_20_24 + CENSOS2011_POP$GE_25_29 + CENSOS2011_POP$GE_30_34 + CENSOS2011_POP$GE_35_39 +
  CENSOS2011_POP$GE_40_44 + CENSOS2011_POP$GE_45_49 + CENSOS2011_POP$GE_50_54 + CENSOS2011_POP$GE_55_59 + CENSOS2011_POP$GE_60_64
CENSOS2011_POP$C11_GEmais65 = CENSOS2011_POP$GE_65_69 + CENSOS2011_POP$GE_70_74 + CENSOS2011_POP$GE_75_79 + CENSOS2011_POP$GE_80_84 +
  CENSOS2011_POP$GE_85_89 + CENSOS2011_POP$GE_90_94 + CENSOS2011_POP$GE_95_99 + CENSOS2011_POP$GE_100e 



####
# * 1.2 PROCESS GEODATA_910111 ----
####

UNION_SUBSECtoNUTSIII.shp = read_sf(dsn = sourceSIG_GEODATA_VARIOS_gdb, layer = "UNION_SUBSECtoNUTSIII_2018_LxCorrigido")
UNION_SUBSECtoNUTSIII = UNION_SUBSECtoNUTSIII.shp %>% st_drop_geometry()

selectCol = c("BGRI11", "BGRI2001", "BGRE1991", 	"DICOFRE12",	"DICOFRE18", 	"AREASUBSEC91", "AREASUBSEC01", 	"AREASUBSEC11", "AREA_UNION_PART", "AREAFRE12v1", "AREAFREG18")

GEODATA_910111 = UNION_SUBSECtoNUTSIII
GEODATA_910111 = GEODATA_910111[,selectCol]

GEODATA_910111$PONDSUBSEC1191 = GEODATA_910111$AREA_UNION_PART / GEODATA_910111$AREASUBSEC91
GEODATA_910111$PONDSUBSEC1101 = GEODATA_910111$AREA_UNION_PART / GEODATA_910111$AREASUBSEC01
GEODATA_910111$PONDSUBSEC1111 = GEODATA_910111$AREA_UNION_PART / GEODATA_910111$AREASUBSEC11
GEODATA_910111$PONDAREAFREG18 = GEODATA_910111$AREA_UNION_PART / GEODATA_910111$AREAFREG18



####
# * 1.3 COMBINE GEODATA&INE_POP_910111  ----
####

GEODATA_910111_POP = GEODATA_910111

GEODATA_910111_POP = merge(GEODATA_910111_POP, CENSOS1991_POP[,c("SUBSEC","C91_GETotal", "C91_GEate20", "C91_GE2065", "C91_GEmais65")], by.x = "BGRE1991" , by.y = "SUBSEC",  all.x = T  )

GEODATA_910111_POP$C91_PopPOND_Total = GEODATA_910111_POP$C91_GETotal * GEODATA_910111_POP$PONDSUBSEC1191
GEODATA_910111_POP$C91_PopPOND_GEate20 = GEODATA_910111_POP$C91_GEate20 * GEODATA_910111_POP$PONDSUBSEC1191
GEODATA_910111_POP$C91_PopPOND_GE2065 = GEODATA_910111_POP$C91_GE2065 * GEODATA_910111_POP$PONDSUBSEC1191
GEODATA_910111_POP$C91_PopPOND_GEmais65 = GEODATA_910111_POP$C91_GEmais65 * GEODATA_910111_POP$PONDSUBSEC1191

GEODATA_910111_POP = merge(GEODATA_910111_POP, CENSOS2001_POP[,c("SUBSEC_EU02","C01_GETotal", "C01_GEate20", "C01_GE2065", "C01_GEmais65")], by.x = "BGRI2001" , by.y = "SUBSEC_EU02",  all.x = T)

GEODATA_910111_POP$C01_PopPOND_Total = GEODATA_910111_POP$C01_GETotal * GEODATA_910111_POP$PONDSUBSEC1101
GEODATA_910111_POP$C01_PopPOND_GEate20 = GEODATA_910111_POP$C01_GEate20 * GEODATA_910111_POP$PONDSUBSEC1101
GEODATA_910111_POP$C01_PopPOND_GE2065 = GEODATA_910111_POP$C01_GE2065 * GEODATA_910111_POP$PONDSUBSEC1101
GEODATA_910111_POP$C01_PopPOND_GEmais65 = GEODATA_910111_POP$C01_GEmais65 * GEODATA_910111_POP$PONDSUBSEC1101

GEODATA_910111_POP = merge(GEODATA_910111_POP, CENSOS2011_POP[,c("SUBSECCAO","C11_GETotal", "C11_GEate20", "C11_GE2065", "C11_GEmais65")], by.x = "BGRI11" , by.y = "SUBSECCAO",  all.x = T)

GEODATA_910111_POP$C11_PopPOND_Total = GEODATA_910111_POP$C11_GETotal * GEODATA_910111_POP$PONDSUBSEC1111
GEODATA_910111_POP$C11_PopPOND_GEate20 = GEODATA_910111_POP$C11_GEate20 * GEODATA_910111_POP$PONDSUBSEC1111
GEODATA_910111_POP$C11_PopPOND_GE2065 = GEODATA_910111_POP$C11_GE2065 * GEODATA_910111_POP$PONDSUBSEC1111
GEODATA_910111_POP$C11_PopPOND_GEmais65 = GEODATA_910111_POP$C11_GEmais65 * GEODATA_910111_POP$PONDSUBSEC1111


col = as.matrix(colnames(GEODATA_910111_POP))
test1 = GEODATA_910111_POP[GEODATA_910111_POP$DICOFRE18 == "090804",  ]
test = GEODATA_910111_POP[GEODATA_910111_POP$DICOFRE18 == "090804", c( "BGRI2001", "PONDSUBSEC1101","AREA_UNION_PART", "AREASUBSEC01", "C01_GETotal" ) ]

####
# * 1.out A   ----
####

OUT.POPCENSOS_2001e2011 = GEODATA_910111_POP %>% 
  group_by( DICOFRE18, FREG18_la ) %>% 
  summarise(
    
    POP2011 = round(sum(C11_PopPOND_Total,  na.rm = TRUE),0),
    POP2001 = round(sum(C01_PopPOND_Total,  na.rm = TRUE),0),
    DensPOP2011 = round(POP2011 / sum(AREA_UNION_PART*10^-6,  na.rm = TRUE),2),
    VarPOP0111 = round((POP2011 - POP2001) / POP2001 , 2)
    
  )

####
# * 1.4 POTENCIAL DEMOGRAFICO   ----
####

CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")
CAOP2018_FREG_Descodifica = CAOP2018_FREG_Descodifica.shp %>% st_drop_geometry()

CAOP2018_FREG_Descodifica$ID = seq(1,nrow(CAOP2018_FREG_Descodifica))
CAOP2018_FREG_Descodifica$raio = sqrt(CAOP2018_FREG_Descodifica$AREAFREG18 / pi)

FREG18_DistMatrix = read_sf(dsn = sourceSIG_GEODATA_VARIOS_gdb, layer = "CAOP2018_FREG_ONEPOLY_pointCentroid_DISTMATRIX")

FREG18_DistMatrixLabels = left_join(FREG18_DistMatrix, CAOP2018_FREG_Descodifica[,c("ID","DICOFRE18")], by.x = "INPUT_FID", by.y = "ID", all.x = T)
colnames(FREG18_DistMatrixLabels)[4] = "INPUT_DICOFRE18"
FREG18_DistMatrixLabels = left_join(FREG18_DistMatrixLabels, CAOP2018_FREG_Descodifica[,c("ID","DICOFRE18")], by = c("NEAR_FID" = "ID"))
colnames(FREG18_DistMatrixLabels)[5] = "NEAR_DICOFRE18"

FREG18_DistMatrixLabels_Wider = FREG18_DistMatrixLabels[, c("DISTANCE", "INPUT_DICOFRE18", "NEAR_DICOFRE18")] %>%
  pivot_wider(names_from = NEAR_DICOFRE18, values_from = DISTANCE)

FREG18_DistMatrixLabels_WiderF = FREG18_DistMatrixLabels_Wider %>% 
  select(sort(current_vars()))




FREG18_DistMatrixLabels_WiderF_Matrix = as.matrix( select(FREG18_DistMatrixLabels_WiderF, -c("INPUT_DICOFRE18")) )
diag(FREG18_DistMatrixLabels_WiderF_Matrix) = CAOP2018_FREG_Descodifica$raio


POP2011byFREG2018 = OUT.POPCENSOS_2001e2011[,c("DICOFRE18", "POP2011")]
POP2011byFREG2018 = POP2011byFREG2018[-1,]

FREG18_DistMatrixLabels_WiderF_MatrixTransf = FREG18_DistMatrixLabels_WiderF_Matrix^2
FREG18_DistMatrixLabels_WiderF_MatrixTransf = 1/FREG18_DistMatrixLabels_WiderF_MatrixTransf

POTDEMO2011byFREG2018_A =  t(as.matrix(POP2011byFREG2018[,2])) %*% FREG18_DistMatrixLabels_WiderF_MatrixTransf  

POTDEMO2011byFREG2018 = tbl_df(cbind(PotDEMO2011 = t(POTDEMO2011byFREG2018_A), FREG2018 = colnames(POTDEMO2011byFREG2018_A))  )
colnames(POTDEMO2011byFREG2018) = c("PotDEMO2011", "DICOFRE18")


####
# * 1.out B   ----
####

OUT.POTDEMO2011byFREG2018 = POTDEMO2011byFREG2018

####
# 2. POIs ----
####



# * 2.1 POIs Amenities ----

poi_amenities_ori_prj_byCAOP2018e2012v1.shp = read_sf(dsn = sourceSIG_POIs_gdb, layer = "poi_amenities_ori_prj_byCAOP2018e2012v1")
poi_amenities_ori_prj_byCAOP2018e2012v1 = poi_amenities_ori_prj_byCAOP2018e2012v1.shp %>% st_drop_geometry()

amenitiesCat = levels(as.factor(poi_amenities_ori_prj_byCAOP2018e2012v1$amenity))

write.table(amenitiesCat, file = paste(OUT.tables.path, "amenitiesCat.txt", sep=""), row.names = F)

amenitiesCat_process = read_delim( file = paste(OUT.tables.path, "amenitiesCat_processed.txt", sep=""), delim = "\t" )

amenitiesCat_process_selection = filter(amenitiesCat_process, amenitiesCat_process$Selection == 1)

poi_amenities_filtered = filter(poi_amenities_ori_prj_byCAOP2018e2012v1,poi_amenities_ori_prj_byCAOP2018e2012v1$amenity  %in% amenitiesCat_process_selection$Category  )

####
# * 2.out A   ----
####

OUT.poi_amenities_filtered_byCAOP2018 = poi_amenities_filtered %>% 
  group_by( DICOFRE18, FREG18_la ) %>% 
  summarise(
    
    NumberOfAmenities = n()
  )


####
# * 2.2 ACESSIBILIDADE   ----
####


poi_amenities_AllCAOP2018 = left_join(CAOP2018_FREG_Descodifica, OUT.poi_amenities_filtered_byCAOP2018, by = c("DICOFRE18" = "DICOFRE18" ))
poi_amenities_AllCAOP2018$NumberOfAmenitiesCorr = poi_amenities_AllCAOP2018$NumberOfAmenities
poi_amenities_AllCAOP2018$NumberOfAmenitiesCorr[is.na(poi_amenities_AllCAOP2018$NumberOfAmenitiesCorr)] = 0

ACESS_ALL_byFREG2018_A =  t(as.matrix(poi_amenities_AllCAOP2018[,c("NumberOfAmenitiesCorr")])) %*% FREG18_DistMatrixLabels_WiderF_MatrixTransf  

ACESS_ALL_byFREG2018 = tbl_df(cbind(ACESS_ALL_byFREG2018 = t(ACESS_ALL_byFREG2018_A), FREG2018 = colnames(POTDEMO2011byFREG2018_A))  )
colnames(ACESS_ALL_byFREG2018) = c("ACESS_ALL", "DICOFRE18")


####
# * 2.out b   ----
####

OUT.ACESS_ALL_byFREG2018 = ACESS_ALL_byFREG2018



####
# 3. COS ----
####

# * 3.1 COS2015 ----

COS2015withCAOP2012v1eCAOP2018.shp = read_sf(dsn = sourceSIG_COS_gdb, layer = "COS2015withCAOP2012v1eCAOP2018")
COS2015withCAOP2012v1eCAOP2018 = COS2015withCAOP2012v1eCAOP2018.shp %>% st_drop_geometry()


COS2015_categorias = COS2015withCAOP2012v1eCAOP2018 %>% 
  group_by( COS2015_V1, COS2015_Le, Megaclasse ) %>% 
  summarise(
    
    AREAT = sum(COS15_AREAPART)
  )

COS2015withCAOP2012v1eCAOP2018$COS2015CAT = COS2015withCAOP2012v1eCAOP2018$COS2015_V1
COS2015withCAOP2018F = COS2015withCAOP2012v1eCAOP2018 %>% separate(COS2015_V1, c("COS15_Nivel1", "COS15_Nivel2", "COS15_Nivel3", "COS15_Nivel4"))


#starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))

COS2015withCAOP2018F_byCategN1_T1 = filter(COS2015withCAOP2018F, COS15_Nivel1 == 1)

OUT.CAOP2018_COS2015_byCategN1_T1 = COS2015withCAOP2018F_byCategN1_T1 %>% 
  group_by( DICOFRE18, FREG18_la ) %>% 
  summarise(
    AREAFREG18_2 = mean(AREAFREG18, rm.na = T),
    AREACategN1_T1 = sum(COS15_AREAPART, rm.na = T),
    PropAREACategN1_T1 = AREACategN1_T1 / AREAFREG18_2
  )

COS2015withCAOP2018F_byCategN1_T2 = filter(COS2015withCAOP2018F, COS15_Nivel1 == 2)

OUT.CAOP2018_COS2015_byCategN1_T2 = COS2015withCAOP2018F_byCategN1_T2 %>% 
  group_by( DICOFRE18, FREG18_la ) %>% 
  summarise(
    AREAFREG18_2 = mean(AREAFREG18, rm.na = T),
    AREACategN1_T2 = sum(COS15_AREAPART, rm.na = T),
    PropAREACategN1_T2 = AREACategN1_T2 / AREAFREG18_2
  )


OUT.CAOP2018_COS2015_byCategN1_T1T2 = left_join(OUT.CAOP2018_COS2015_byCategN1_T1[-1,], OUT.CAOP2018_COS2015_byCategN1_T2, by = c("DICOFRE18" = "DICOFRE18"))

# * 3.out A ----

OUT.CAOP2018_COS2015_byCategN1_T1T2 = OUT.CAOP2018_COS2015_byCategN1_T1T2[ , c(1,4,5,8,9)]




####
# 0. SAVE GERAL ----
####



save(OUT.POPCENSOS_2001e2011,
     OUT.POTDEMO2011byFREG2018, 
     OUT.ACESS_ALL_byFREG2018, 
     OUT.CAOP2018_COS2015_byCategN1_T1T2,
     file = "OUT.TRANSFURBANAS.PREPROCESSING.V01.RData" )
