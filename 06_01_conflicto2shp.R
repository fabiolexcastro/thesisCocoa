# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
adm <- st_read('../data/shp/bse/mpios_geo_ok.shp') %>% 
  mutate(ID_ESPACIA = as.character(ID_ESPACIA) %>% as.numeric())
tbl <- read_excel('../data/tbl/conflicto/CONFLICTO.xlsx')

# Change the colnames
colnames(tbl) <- c('divipola1', 'divipola2', 'dpto', 'mpio', 'confl_num', 'confl_clt', 'region')

# Processing Tumaco -------------------------------------------------------
tumaco <- adm %>% filter(ID_ESPACIA == 52835)
tumaco_tbl <- tbl %>% filter(divipola1 == 52835)
tumaco <- inner_join(tumaco, tumaco_tbl, by = c('ID_ESPACIA' = 'divipola1'))

# Join between the table and the shapefile --------------------------------
adm_tbl <- inner_join(adm, tbl, by = c('ID_ESPACIA' = 'divipola1'))
adm_tbl <- rbind(adm_tbl, tumaco)
antijoin <- anti_join(adm, tbl, by = c('ID_ESPACIA' = 'divipola1'))
st_write(adm_tbl, dsn = '../data/shp/cfl', layer = 'conflicto1', driver = 'ESRI Shapefile', update = TRUE)
