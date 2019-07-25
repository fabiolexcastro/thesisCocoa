

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tbl <- read_csv('../data/tbl/prd/production_tidy_all.csv')
adm <- st_read('../data/shp/bse/mpios_geo_ok.shp') %>% 
  mutate(ID_ESPACIA = as.character(ID_ESPACIA) %>% as.numeric())

# Summaryzing the cocoa table ---------------------------------------------
count <- tbl %>% 
  group_by(PERIODO) %>% 
  summarize(count = n()) %>% 
  ungroup()

# Plot of count cocoa municipalities  
gg_count <- ggplot(count, aes(PERIODO, count)) +
  geom_line() +
  scale_x_continuous(name = 'Periodo',
                     breaks = seq(2007, 2017, 2)) +
  ylab('Cantidad de municipios') +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
ggsave(plot = gg_count, filename = '../png/grp/prd/count_mpios.png', units = 'cm', width = 9, height = 6, dpi = 300)

# Summarize the production table ------------------------------------------
smm <- tbl %>% 
  group_by(COD_MUNI, DPTO, MPIO) %>% 
  summarise(AREA_SEMBRADA = mean(AREA_SEMBRADA_HA, na.rm = TRUE),
            AREA_COSECHADA = mean(AREA_COSECHADA_HA, na.rm = TRUE),
            PRODUCCION = mean(PRODUCCION, na.rm = TRUE),
            RDTOS = mean(RDTOS, na.rm = TRUE)) %>% 
  ungroup()

adm_cca <- inner_join(adm, smm, by = c('ID_ESPACIA' = 'COD_MUNI'))
st_write(adm_cca, dsn = '../data/shp/prd', layer = 'cacao_avg_07_17', driver = 'ESRI Shapefile', update = TRUE)
