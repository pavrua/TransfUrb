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

library("reshape2") 
#reshape2 :: usa as funções dcast e melt equivalents para fazer pivot tables
# no tidyverse temos o  tidyr com as funcoes pivot_



####
# 0. DIRECTORIES ----
####

#PC UA
pathUA = "C:/Users/pauloricardolb/Universidade de Aveiro/OP_DRIVIT-UP - Documentos/"

path = pathUA

setwd(paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/TransfUrb/PreProcessing", sep=""))


OUT.tables.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.tables/", sep = "")

OUT.RData.path = paste(path, "02_TRABALHOS_DESENVOLVIDOS/023_DSS_COMPONENTES/0230_DEMOG_ECON/02301_PadroesTransfUrbana_JAN/MODELACAO/OUT.RData/", sep = "")


sourceTABLE_demografia = paste(path, "01_BASESDEDADOS/TABELAS/DEMOGRAFIA/POPULACAO_GE5anos_CENSOS_91_01_11/", sep="")
sourceTABLE_demografia.projeccoesPPF = paste(path, "01_BASESDEDADOS/TABELAS/DEMOGRAFIA/POPULACAO_GE5anos_PROJECCOES/POP_FECHADAS_N5_V20190207/", sep="")

sourceTABLE_CAOP12v1CAOP18_DESCODIF = paste(path, "01_BASESDEDADOS/TABELAS/CAOP_DESCODIFICA/", sep="")


sourceSIG_ADMIN_CAOP_BASES_gdb = paste(path, "01_BASESDEDADOS/SIG/BASES_ADMIN_NACIONAL/ADMIN_CAOP_BASES.gdb", sep = "")
sourceSIG_GEODATA_VARIOS_gdb = paste(path, "01_BASESDEDADOS/SIG/GEODATA_VARIOS.gdb", sep = "")






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
CENSOS1991_POP$C91_GE2040 = CENSOS1991_POP$GE_20_24 + CENSOS1991_POP$GE_25_29 + CENSOS1991_POP$GE_30_34 + CENSOS1991_POP$GE_35_39
CENSOS1991_POP$C91_GE4065 = CENSOS1991_POP$GE_40_44 + CENSOS1991_POP$GE_45_49 + CENSOS1991_POP$GE_50_54 + CENSOS1991_POP$GE_55_59 + CENSOS1991_POP$GE_60_64
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
CENSOS2001_POP$C01_GE2040 = CENSOS2001_POP$GE_20_24 + CENSOS2001_POP$GE_25_29 + CENSOS2001_POP$GE_30_34 + CENSOS2001_POP$GE_35_39
CENSOS2001_POP$C01_GE4065 = CENSOS2001_POP$GE_40_44 + CENSOS2001_POP$GE_45_49 + CENSOS2001_POP$GE_50_54 + CENSOS2001_POP$GE_55_59 + CENSOS2001_POP$GE_60_64
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
CENSOS2011_POP$C11_GE2040 = CENSOS2011_POP$GE_20_24 + CENSOS2011_POP$GE_25_29 + CENSOS2011_POP$GE_30_34 + CENSOS2011_POP$GE_35_39
CENSOS2011_POP$C11_GE4065 = CENSOS2011_POP$GE_40_44 + CENSOS2011_POP$GE_45_49 + CENSOS2011_POP$GE_50_54 + CENSOS2011_POP$GE_55_59 + CENSOS2011_POP$GE_60_64
CENSOS2011_POP$C11_GEmais65 = CENSOS2011_POP$GE_65_69 + CENSOS2011_POP$GE_70_74 + CENSOS2011_POP$GE_75_79 + CENSOS2011_POP$GE_80_84 +
  CENSOS2011_POP$GE_85_89 + CENSOS2011_POP$GE_90_94 + CENSOS2011_POP$GE_95_99 + CENSOS2011_POP$GE_100e 



####
# * 1.2 PROCESS GEODATA_910111 ----
####

INTERSECT_SUBSEC910111toNUTS2016.shp = read_sf(dsn = sourceSIG_GEODATA_VARIOS_gdb, layer = "INTERSECT_SUBSEC910111toNUTS2016")
INTERSECT_SUBSEC910111toNUTS2016 = INTERSECT_SUBSEC910111toNUTS2016.shp %>% st_drop_geometry()

#selectCol = c("BGRI11", "BGRI2001", "BGRE1991", 	"DICOFRE12",	"DICOFRE18", 	"AREASUBSEC91", "AREASUBSEC01", 	"AREASUBSEC11", "AREA_UNION_PART", "AREAFRE12v1", "AREAFREG18")

GEODATA_910111 = INTERSECT_SUBSEC910111toNUTS2016


GEODATA_910111$PONDSUBSEC1191 = GEODATA_910111$INTERSECT_AREAPART / GEODATA_910111$AREASUBSEC91
GEODATA_910111$PONDSUBSEC1101 = GEODATA_910111$INTERSECT_AREAPART / GEODATA_910111$AREASUBSEC01
GEODATA_910111$PONDSUBSEC1111 = GEODATA_910111$INTERSECT_AREAPART / GEODATA_910111$AREASUBSEC11
GEODATA_910111$PONDAREAFREG18 = GEODATA_910111$INTERSECT_AREAPART / GEODATA_910111$AREAFREG18



####
# * 1.3 COMBINE GEODATA&INE_POP_910111  ----
####

GEODATA_910111_POP = GEODATA_910111

GEODATA_910111_POP = merge(GEODATA_910111_POP, CENSOS1991_POP[,c("SUBSEC","C91_GETotal", "C91_GEate20", "C91_GE2040", "C91_GE4065", "C91_GEmais65")], by.x = "BGRE1991" , by.y = "SUBSEC",  all.x = T  )

GEODATA_910111_POP$C91_PopPOND_Total = GEODATA_910111_POP$C91_GETotal * GEODATA_910111_POP$PONDSUBSEC1191
GEODATA_910111_POP$C91_PopPOND_GEate20 = GEODATA_910111_POP$C91_GEate20 * GEODATA_910111_POP$PONDSUBSEC1191
GEODATA_910111_POP$C91_PopPOND_GE2040 = GEODATA_910111_POP$C91_GE2040 * GEODATA_910111_POP$PONDSUBSEC1191
GEODATA_910111_POP$C91_PopPOND_GE4065 = GEODATA_910111_POP$C91_GE4065 * GEODATA_910111_POP$PONDSUBSEC1191
GEODATA_910111_POP$C91_PopPOND_GEmais65 = GEODATA_910111_POP$C91_GEmais65 * GEODATA_910111_POP$PONDSUBSEC1191

GEODATA_910111_POP = merge(GEODATA_910111_POP, CENSOS2001_POP[,c("SUBSEC_EU02","C01_GETotal", "C01_GEate20", "C01_GE2040", "C01_GE4065","C01_GEmais65")], by.x = "BGRI2001_Corr" , by.y = "SUBSEC_EU02",  all.x = T)

GEODATA_910111_POP$C01_PopPOND_Total = GEODATA_910111_POP$C01_GETotal * GEODATA_910111_POP$PONDSUBSEC1101
GEODATA_910111_POP$C01_PopPOND_GEate20 = GEODATA_910111_POP$C01_GEate20 * GEODATA_910111_POP$PONDSUBSEC1101
GEODATA_910111_POP$C01_PopPOND_GE2040 = GEODATA_910111_POP$C01_GE2040 * GEODATA_910111_POP$PONDSUBSEC1101
GEODATA_910111_POP$C01_PopPOND_GE4065 = GEODATA_910111_POP$C01_GE4065 * GEODATA_910111_POP$PONDSUBSEC1101
GEODATA_910111_POP$C01_PopPOND_GEmais65 = GEODATA_910111_POP$C01_GEmais65 * GEODATA_910111_POP$PONDSUBSEC1101

GEODATA_910111_POP = merge(GEODATA_910111_POP, CENSOS2011_POP[,c("SUBSECCAO","C11_GETotal", "C11_GEate20", "C11_GE2040", "C11_GE4065","C11_GEmais65")], by.x = "BGRI11" , by.y = "SUBSECCAO",  all.x = T)

GEODATA_910111_POP$C11_PopPOND_Total = GEODATA_910111_POP$C11_GETotal * GEODATA_910111_POP$PONDSUBSEC1111
GEODATA_910111_POP$C11_PopPOND_GEate20 = GEODATA_910111_POP$C11_GEate20 * GEODATA_910111_POP$PONDSUBSEC1111
GEODATA_910111_POP$C11_PopPOND_GE2040 = GEODATA_910111_POP$C11_GE2040 * GEODATA_910111_POP$PONDSUBSEC1111
GEODATA_910111_POP$C11_PopPOND_GE4065 = GEODATA_910111_POP$C11_GE4065 * GEODATA_910111_POP$PONDSUBSEC1111
GEODATA_910111_POP$C11_PopPOND_GEmais65 = GEODATA_910111_POP$C11_GEmais65 * GEODATA_910111_POP$PONDSUBSEC1111

####
# * 1.out A   ----
####

OUT.POPCENSOS_2001e2011 = GEODATA_910111_POP %>% 
  group_by( DICOFRE18, FREG18_la ) %>% 
  summarise(
    
    POP_C2001_Total = round(sum(C01_PopPOND_Total,  na.rm = TRUE),0),
    POP_C2001_GEate20 = round(sum(C01_PopPOND_GEate20,  na.rm = TRUE),0),
    POP_C2001_GE2040 = round(sum(C01_PopPOND_GE2040,  na.rm = TRUE),0),
    POP_C2001_GE4045 = round(sum(C01_PopPOND_GE4065,  na.rm = TRUE),0),
    POP_C2001_GEmais65 = round(sum(C01_PopPOND_GEmais65,  na.rm = TRUE),0),
    
    POP_C2011_Total = round(sum(C11_PopPOND_Total,  na.rm = TRUE),0),
    POP_C2011_GEate20 = round(sum(C11_PopPOND_GEate20,  na.rm = TRUE),0),
    POP_C2011_GE2040 = round(sum(C11_PopPOND_GE2040,  na.rm = TRUE),0),
    POP_C2011_GE4045 = round(sum(C11_PopPOND_GE4065,  na.rm = TRUE),0),
    POP_C2011_GEmais65 = round(sum(C11_PopPOND_GEmais65,  na.rm = TRUE),0),
    
    POP_C2011_DensPop = round(POP_C2011_Total / sum(INTERSECT_AREAPART*10^-6,  na.rm = TRUE),2),
    POP_VarPop_C2001C2011 = round((POP_C2011_Total - POP_C2001_Total) / POP_C2001_Total , 2),
    
    N_LUGARESbyFREG = n_distinct(LUG11)
    
  )

####
# * 1.4 ADD PROJECCOES PPF ----
####

# * * 1.4.1 GET DATA  ----
PPF_BD_N5_111117 = read_table2(file = paste(sourceTABLE_demografia.projeccoesPPF, "BD_N5 111-117.txt", sep = "") )
PPF_BD_N5_118164 = read_table2(file = paste(sourceTABLE_demografia.projeccoesPPF, "BD_N5 118-164.txt", sep = "") )
PPF_BD_N5_165193 = read_table2(file = paste(sourceTABLE_demografia.projeccoesPPF, "BD_N5 165-193.txt", sep = "") )
PPF_BD_N5_171150 = read_table2(file = paste(sourceTABLE_demografia.projeccoesPPF, "BD_N5 171-150.txt", sep = "") )

PPF_BD_N5 = rbind(PPF_BD_N5_111117, PPF_BD_N5_118164, PPF_BD_N5_165193, PPF_BD_N5_171150)

PPF_BD_N5$ID <- seq.int(nrow(PPF_BD_N5))

PPF_BD_N5$PopAbertaHM = PPF_BD_N5$PopAbertaH + PPF_BD_N5$PopAbertaM

PPF_BD_N5$GE_match = as.character(PPF_BD_N5$GE)


# * * 1.4.2 DESCODIFICA GE   ----

GE_ori = as.character(unique(PPF_BD_N5$GE))
GE_agrega = c("GEate20", "GEate20", "GEate20", "GEate20", "GE2040", "GE2040", "GE2040", "GE2040", "GE4065", "GE4065", "GE4065", "GE4065", "GE4065", "GEmais65" , "GEmais65", "GEmais65", "GEmais65", "GEmais65", "GEmais65", "GEmais65", "GEmais65")
# Forma "a martelo" de inserir a designação dos grupos etários
DESCODIFICA_GE = data.frame(GE_ori, GE_agrega, stringsAsFactors = F)

PPF_BD_N5_DESC = left_join(PPF_BD_N5, DESCODIFICA_GE, by = c("GE_match" = "GE_ori"))



# * * 1.4.3 PPF ALL FREG12 N5   ----

#Nota: para inserir zeros à esquerda o package stringr tem a função str_pad


PPF_BD_N5_DESC$N3_cod = str_pad(PPF_BD_N5_DESC$N3, width = 3, side = "left", pad = "0")
PPF_BD_N5_DESC$N2_cod = substr(PPF_BD_N5_DESC$N3_cod, 1,2)
PPF_BD_N5_DESC$N5_cod = str_pad(PPF_BD_N5_DESC$N5, width = 2, side = "left", pad = "0")
PPF_BD_N5_DESC$N4_cod = str_pad(PPF_BD_N5_DESC$N4, width = 4, side = "left", pad = "0")
PPF_BD_N5_DESC$N4N5_cod = paste(PPF_BD_N5_DESC$N4_cod, PPF_BD_N5_DESC$N5_cod, sep = "")


PPF_BD_N5_POPHM_LONGFORMAT = dcast(PPF_BD_N5_DESC, N4N5_cod ~ ano + GE_agrega, value.var = c("PopAbertaHM"), sum)
colnames(PPF_BD_N5_POPHM_LONGFORMAT) = c("FREG12_N4N5_cod", 
                                         "FREG12_POPHM_2010_GE2040", "FREG12_POPHM_2010_GE4065",  "FREG12_POPHM_2010_GEate20", "FREG12_POPHM_2010_GEmais65",
                                         "FREG12_POPHM_2015_GE2040", "FREG12_POPHM_2015_GE4065",  "FREG12_POPHM_2015_GEate20", "FREG12_POPHM_2015_GEmais65",
                                         "FREG12_POPHM_2020_GE2040", "FREG12_POPHM_2020_GE4065",  "FREG12_POPHM_2020_GEate20", "FREG12_POPHM_2020_GEmais65",
                                         "FREG12_POPHM_2025_GE2040", "FREG12_POPHM_2025_GE4065",  "FREG12_POPHM_2025_GEate20", "FREG12_POPHM_2025_GEmais65",
                                         "FREG12_POPHM_2030_GE2040", "FREG12_POPHM_2030_GE4065",  "FREG12_POPHM_2030_GEate20", "FREG12_POPHM_2030_GEmais65",
                                         "FREG12_POPHM_2035_GE2040", "FREG12_POPHM_2035_GE4065",  "FREG12_POPHM_2035_GEate20", "FREG12_POPHM_2035_GEmais65",
                                         "FREG12_POPHM_2040_GE2040", "FREG12_POPHM_2040_GE4065",  "FREG12_POPHM_2040_GEate20", "FREG12_POPHM_2040_GEmais65")


# * * 1.4.4 DESCODIFICA GEO POND POP N5   ----

GEODESCODIFICAFREG12FREG18 = read_delim(file = paste(sourceTABLE_CAOP12v1CAOP18_DESCODIF, "DESCODIFICA_FREG121_toFREG18MUNI18NUTS2016.txt", sep = ""), delim = "," )

GEODESCODIFICAFREG12FREG18_CLEAN = GEODESCODIFICAFREG12FREG18[!is.na(GEODESCODIFICAFREG12FREG18$DICOFRE12), ]
GEODESCODIFICAFREG12FREG18_CLEAN = GEODESCODIFICAFREG12FREG18_CLEAN[!is.na(GEODESCODIFICAFREG12FREG18_CLEAN$DICOFRE18), ]
GEODESCODIFICAFREG12FREG18_CLEAN$POND_AREA_FREG12 = GEODESCODIFICAFREG12FREG18_CLEAN$APARTFREG / GEODESCODIFICAFREG12FREG18_CLEAN$AFREG12



PPF_BD_N5_POPHM_LONGFORMAT_ALL = left_join(GEODESCODIFICAFREG12FREG18_CLEAN, PPF_BD_N5_POPHM_LONGFORMAT, by = c("DICOFRE12" = "FREG12_N4N5_cod"))
PPF_BD_N5_POPHM_LONGFORMAT_ALL$AUXID = seq(1, nrow(PPF_BD_N5_POPHM_LONGFORMAT_ALL))

PPF_BD_N5_POPHM_LONGFORMAT_ALL_POND = as.data.frame(sapply( PPF_BD_N5_POPHM_LONGFORMAT_ALL[,c(19:46)], FUN = function(xxx ){ (PPF_BD_N5_POPHM_LONGFORMAT_ALL[,c("POND_AREA_FREG12")] * xxx) } ) )
colnames(PPF_BD_N5_POPHM_LONGFORMAT_ALL_POND) = c( 
  "FREG18part_POPHM_2010_GE2040", "FREG18part_POPHM_2010_GE4065",  "FREG18part_POPHM_2010_GEate20", "FREG18part_POPHM_2010_GEmais65",
  "FREG18part_POPHM_2015_GE2040", "FREG18part_POPHM_2015_GE4065",  "FREG18part_POPHM_2015_GEate20", "FREG18part_POPHM_2015_GEmais65",
  "FREG18part_POPHM_2020_GE2040", "FREG18part_POPHM_2020_GE4065",  "FREG18part_POPHM_2020_GEate20", "FREG18part_POPHM_2020_GEmais65",
  "FREG18part_POPHM_2025_GE2040", "FREG18part_POPHM_2025_GE4065",  "FREG18part_POPHM_2025_GEate20", "FREG18part_POPHM_2025_GEmais65",
  "FREG18part_POPHM_2030_GE2040", "FREG18part_POPHM_2030_GE4065",  "FREG18part_POPHM_2030_GEate20", "FREG18part_POPHM_2030_GEmais65",
  "FREG18part_POPHM_2035_GE2040", "FREG18part_POPHM_2035_GE4065",  "FREG18part_POPHM_2035_GEate20", "FREG18part_POPHM_2035_GEmais65",
  "FREG18part_POPHM_2040_GE2040", "FREG18part_POPHM_2040_GE4065",  "FREG18part_POPHM_2040_GEate20", "FREG18part_POPHM_2040_GEmais65")



PPF_BD_N5_POPHM_LONGFORMAT_FINAL = cbind(as.data.frame(PPF_BD_N5_POPHM_LONGFORMAT_ALL), PPF_BD_N5_POPHM_LONGFORMAT_ALL_POND)


####
# * 1.out B   ----
####

OUT.POP_PPF = PPF_BD_N5_POPHM_LONGFORMAT_FINAL %>% 
  group_by( DICOFRE18, FREG18_la ) %>% 
  summarise(
    
    POP_PPF_2015_Total = round(sum(FREG18part_POPHM_2015_GEate20,  na.rm = TRUE),0) +
      round(sum(FREG18part_POPHM_2015_GE2040,  na.rm = TRUE),0) +
      round(sum(FREG18part_POPHM_2015_GE4065,  na.rm = TRUE),0) +
      round(sum(FREG18part_POPHM_2015_GEmais65,  na.rm = TRUE),0),
    
    POP_PPF_2015_GEate20 = round(sum(FREG18part_POPHM_2015_GEate20,  na.rm = TRUE),0),
    POP_PPF_2015_GE2040 = round(sum(FREG18part_POPHM_2015_GE2040,  na.rm = TRUE),0),
    POP_PPF_2015_GE4065 = round(sum(FREG18part_POPHM_2015_GE4065,  na.rm = TRUE),0),
    POP_PPF_2015_GEmais65 = round(sum(FREG18part_POPHM_2015_GEmais65,  na.rm = TRUE),0)
    
  )



####
# * 1.5 POTENCIAL DEMOGRAFICO 2011   ----
####

CAOP2018_FREG_Descodifica.shp = read_sf(dsn = sourceSIG_ADMIN_CAOP_BASES_gdb, layer = "CAOP2018_FREG_ONEPOLY_LxCORRIGIDO")
CAOP2018_FREG_Descodifica = CAOP2018_FREG_Descodifica.shp %>% st_drop_geometry()

CAOP2018_FREG_Descodifica$ID = seq(1,nrow(CAOP2018_FREG_Descodifica))
CAOP2018_FREG_Descodifica$raio = sqrt(CAOP2018_FREG_Descodifica$AREAFREG18 / pi)

#FREG18_DistMatrix = read_sf(dsn = sourceSIG_GEODATA_VARIOS_gdb, layer = "CAOP2018_FREG_ONEPOLY_pointCentroid_DISTMATRIX")
FREG18_DistMatrix = read_sf(dsn = sourceSIG_GEODATA_VARIOS_gdb, layer = "FREG_CAOP2018_pointCentroid_LXAVRILH_CORRECT_DISTMATRIX")


FREG18_DistMatrixLabels = merge(FREG18_DistMatrix, CAOP2018_FREG_Descodifica[,c("ID","DICOFRE18")], by.x ="INPUT_FID" , by.y = "ID", all.x = T)
colnames(FREG18_DistMatrixLabels)[4] = "INPUT_DICOFRE18"
FREG18_DistMatrixLabels = merge(FREG18_DistMatrixLabels, CAOP2018_FREG_Descodifica[,c("ID","DICOFRE18")], by.x = "NEAR_FID" , by.y = "ID", all.x = T)
colnames(FREG18_DistMatrixLabels)[5] = "NEAR_DICOFRE18"

FREG18_DistMatrixLabels_Wider = FREG18_DistMatrixLabels[, c("DISTANCE", "INPUT_DICOFRE18", "NEAR_DICOFRE18")] %>%
  pivot_wider(names_from = NEAR_DICOFRE18, values_from = DISTANCE)

FREG18_DistMatrixLabels_WiderF = FREG18_DistMatrixLabels_Wider[order(FREG18_DistMatrixLabels_Wider$INPUT_DICOFRE18), ]
#FREG18_DistMatrixLabels_WiderF = FREG18_DistMatrixLabels_Wider %>% 
#  select(sort(current_vars()))

FREG18_DistMatrixLabels_WiderF_Matrix = as.matrix( select(FREG18_DistMatrixLabels_WiderF, -c("INPUT_DICOFRE18")) )
diag(FREG18_DistMatrixLabels_WiderF_Matrix) = CAOP2018_FREG_Descodifica$raio


POP2011byFREG2018 = OUT.POPCENSOS_2001e2011[,c("DICOFRE18", "POP_C2011_Total")]

FREG18_DistMatrixLabels_WiderF_MatrixTransf = FREG18_DistMatrixLabels_WiderF_Matrix*10^-3
FREG18_DistMatrixLabels_WiderF_MatrixTransf = (FREG18_DistMatrixLabels_WiderF_MatrixTransf)^2
FREG18_DistMatrixLabels_WiderF_MatrixTransf = 1/FREG18_DistMatrixLabels_WiderF_MatrixTransf

POTDEMO2011byFREG2018_A =  t(as.matrix(POP2011byFREG2018[,2])) %*%  FREG18_DistMatrixLabels_WiderF_MatrixTransf   

POTDEMO2011byFREG2018 = data.frame(DICOFRE18 = colnames(POTDEMO2011byFREG2018_A), PotDEMO2011 = as.numeric(t(POTDEMO2011byFREG2018_A))  )



####
# * 1.out C   ----
####

OUT.POTDEMO2011byFREG2018 = POTDEMO2011byFREG2018







####
# 0. SAVE GERAL ----
####

BD_DEMOALL = left_join(CAOP2018_FREG_Descodifica, OUT.POPCENSOS_2001e2011, by = c("DICOFRE18" = "DICOFRE18"))
BD_DEMOALL = left_join(BD_DEMOALL, OUT.POTDEMO2011byFREG2018, by = c("DICOFRE18" = "DICOFRE18"))
BD_DEMOALL = left_join(BD_DEMOALL, OUT.POP_PPF, by = c("DICOFRE18" = "DICOFRE18"))

BD_DEMOALL$POP_VarPop_C2011C2015 = (BD_DEMOALL$POP_PPF_2015_Total - BD_DEMOALL$POP_C2011_Total) / BD_DEMOALL$POP_C2011_Total

#BD_TRANSFURB$DensPOP2011_TerritArtif = BD_TRANSFURB$POP_C2011_Total / BD_TRANSFURB$AREACategN1_T1

BD_DEMOALL[is.na(BD_DEMOALL)] = 0 


save(BD_DEMOALL,
     OUT.POPCENSOS_2001e2011,
     OUT.POTDEMO2011byFREG2018,
     OUT.POP_PPF, 
     file = paste(OUT.RData.path, "DEMOGRAFIA_01_11_15_CAOP2018.RData", sep="") )
