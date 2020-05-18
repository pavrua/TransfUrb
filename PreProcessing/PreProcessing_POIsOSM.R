library(tidyverse)
library(osmdata)
library(sf)
#library(ggmap)


head(available_features())
head(available_tags("amenity"))
head(available_tags("shop"))



#bounding box for Portugal https://www.openstreetmap.org/export#map=6/39.623/-8.459
m <- c(-9.602, 36.932, -6.152, 42.180)

#building the query
q <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity") 


dados = osmdata_sf(q)


dados = osmdata_xml(q)

xml_doc = dados


library(XML)
myXML = xmlParse(dados)
myData = xmlToDataFrame(myXML, stringsAsFactors = FALSE,) %>% 
  mutate_all(~type.convert(., as.is = T))


library(xml2)
doc <- as_list(read_xml(xml_doc$node))
str(doc$DATA, 1)

as_tibble(doc) %>%
  # new tidyr function
  unnest_wider(DATA) %>%
  # unnest same length list cols
  unnest(cols = names(.)) %>%
  unnest(cols = names(.)) %>%
  # convert using readr parser
  readr::type_convert()


