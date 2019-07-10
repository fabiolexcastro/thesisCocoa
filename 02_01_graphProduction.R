

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, readxl, raster, rgdal, sf, rgeos, velox, ggpubr)

# Initial setup -----------------------------------------------------------
g <- gc(reset = T)
rm(list = ls())
options(sipen = 999)

# Load data ---------------------------------------------------------------
adm <- shapefile('../data/shp/bse/mpios_geo_ok.shp')
prd <- read_csv('../data/tbl/prd/production_tidy_all.csv')

# Filter and summarize
smr <- prd %>%
  dplyr::group_by(PERIODO) %>%
  dplyr::summarize(area_sembrada = sum(AREA_SEMBRADA_HA, na.rm = TRUE), 
                   area_cosechada = sum(AREA_COSECHADA_HA, na.rm = TRUE), 
                   produccion = sum(PRODUCCION, na.rm = TRUE), 
                   RDTOS = mean(RDTOS, na.rm = TRUE)) %>%
  dplyr::ungroup()
smr <- smr %>% 
  gather(Variable, value, -PERIODO)
unique(smr$Variable)

smr$Variable <- gsub('area_cosechada', 'Area cosechada', smr$Variable)
smr$Variable <- gsub('area_sembrada', 'Area sembrada', smr$Variable)
smr$Variable <- gsub('RDTOS', 'Rendimientos', smr$Variable)
smr$Variable <- gsub('produccion', 'Produccion', smr$Variable)

# Area sembrada y cosechada
gg1 <- ggplot(data = smr %>% filter(Variable %in% c('Area sembrada', 'Area cosechada')), aes(x = factor(PERIODO), y = value, group = Variable, col = Variable)) +
  geom_line(size = 1.3) +
  theme(legend.position = 'top') +
  labs(fill = '') +
  xlab('Año') +
  ylab('Hectáreas') +
  scale_color_manual(values = c('#8B8A8A', '#0C0C0C')) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ',', scientific = FALSE))
ggsave(plot = gg1, filename = '../png/grp/prd/crp_hrv_07_17_line.png', height = 6, width = 9, units = 'in', dpi = 300)

# ProducciÃ³n
gg2 <- ggplot(data = smr %>% filter(Variable %in% 'Produccion'), aes(x = factor(PERIODO), y = value, group = 1)) +
  geom_line(size = 1.3) +
  theme(legend.position = 'top') +
  labs(fill = '') +
  xlab('Año') +
  ylab('Producción (Ton)') +
  scale_color_manual(values = c('#8B8A8A')) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ',', scientific = FALSE))
ggsave(plot = gg2, filename = '../png/grp/prd/prd_07_17_line.png', height = 6, width = 9, units = 'in', dpi = 300)

# Rendimientos
gg3 <- ggplot(data = smr %>% filter(Variable %in% 'Rendimientos'), aes(x = factor(PERIODO), y = value, group = 1)) +
  geom_line(size = 1.3) +
  theme(legend.position = 'top') +
  labs(fill = '') +
  xlab('Año') +
  ylab('Rendimientos (Ton / Ha)') +
  scale_color_manual(values = c('#8B8A8A')) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ',', scientific = FALSE))
ggsave(plot = gg3, filename = '../png/grp/prd/yld_07_17_line.png', height = 6, width = 9, units = 'in', dpi = 300)

gg <- ggarrange(gg1, gg2, gg3, labels = c('A', 'B', 'C'), ncol = 1, nrow = 3)

ggsave(plot = gg, filename = '../png/grp/prd/crp_prd_yld.png', height = 22, width = 14, units = 'cm', dpi = 300)
