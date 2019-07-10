

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, stringr, sf, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
zmc <- read_csv('../data/tbl/conflicto/ZOMAC.csv') %>% 
  setNames(c('CodigoDANE', 'Departamento', 'Municipio', 'Subregion')) %>% 
  mutate(Departamento = iconv(Departamento, to = 'latin1'),
         Municipio = iconv(Municipio, to = 'latin1'),
         Subregion = iconv(Subregion, to = 'latin1'))

pdt <- read_csv('../data/tbl/conflicto/PDET.csv') %>% 
  dplyr::select(-UBICACION)

shp <- st_read('../data/shp/bse/mpios_geo.shp')

# Review duplicated in shapefile ------------------------------------------
duplicated(shp$ID_ESPACIA) %>% unique()
codes <- shp[duplicated(shp$ID_ESPACIA),] %>% 
  pull(ID_ESPACIA) %>% 
  as.character() %>% 
  as.numeric()
shp1 <- shp %>% 
  filter(!ID_ESPACIA %in% codes) %>% 
  dplyr::select(ID_ESPACIA, OBJECTID, NOM_MUNICI, COD_DEPTO, NOMBRE_DPT)
shp2 <- shp %>% 
  filter(ID_ESPACIA %in% codes)
tb2 <- shp %>% 
  filter(ID_ESPACIA %in% codes) %>% 
  as.data.frame() %>% 
  dplyr::select(OBJECTID, ID_ESPACIA, NOM_MUNICI, COD_DEPTO, NOMBRE_DPT) %>% 
  mutate(ID_ESPACIA = as.numeric(as.character(ID_ESPACIA))) 
names <- tb2[!duplicated(tb2$ID_ESPACIA),] %>% 
  pull(3) %>% 
  as.character()
tb2 <- tb2 %>% 
  filter(NOM_MUNICI %in% names)
shp2 <- st_as_sf(aggregate(as(shp2, 'Spatial'), 'ID_ESPACIA')) %>% 
  mutate(ID_ESPACIA = as.numeric(as.character(ID_ESPACIA))) %>% 
  inner_join(., tb2, by = c('ID_ESPACIA' = 'ID_ESPACIA'))

colnames(shp1)
colnames(shp2)

shp <- rbind(shp1, shp2)
st_write(obj = shp, dsn = '../data/shp/bse', layer = 'mpio_geo_ok', driver = 'ESRI Shapefile', update = TRUE)

# Join between shapefile and table ----------------------------------------
sft <- st_as_sf(shp) %>% 
  mutate(ID_ESPACIA = as.numeric(as.character(ID_ESPACIA)))
zmc_shp <- inner_join(sft, zmc, by = c('ID_ESPACIA' = 'CodigoDANE'))
pdt_shp <- inner_join(sft, pdt, by = c('ID_ESPACIA' = 'COD_DANE_MPIO'))  

# Write the final shapefiles ----------------------------------------------
st_write(obj = zmc_shp, dsn = '../data/shp/cfl', layer = 'zomac', driver = 'ESRI Shapefile', update = TRUE)
st_write(obj = pdt_shp, dsn = '../data/shp/cfl', layer = 'pdet', driver = 'ESRI Shapefile', update = TRUE)


as.data.frame(zmc_shp) %>% 
  group_by(Departamento) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

as.data.frame(pdt_shp) %>% 
  group_by(NOMBRE_DPT) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))


tbl_zmc <- as.data.frame(zmc_shp) %>% 
  as_tibble() %>% 
  dplyr::select(ID_ESPACIA, NOMBRE_DPT, Municipio)

tbl_pdt <- as.data.frame(pdt_shp) %>% 
  as_tibble() %>% 
  dplyr::select(ID_ESPACIA, NOMBRE_DPT, MUNICIPIO)

inner_join(tbl_zmc, tbl_pdt, by = 'ID_ESPACIA')
anti_join(tbl_zmc, tbl_pdt, by = 'ID_ESPACIA')















