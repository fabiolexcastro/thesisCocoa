
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, readxl, raster, rgdal, sf, rgeos, velox, ggpubr)

# Initial setup -----------------------------------------------------------
g <- gc(reset = T)
rm(list = ls())
options(sipen = 999)

# Functions to use --------------------------------------------------------
empty_as_na <- function(x){
  if('factor' %in% class(x)) x <- as.character(x) 
  ifelse(as.character(x)!='', x, NA)
}

# Load data ---------------------------------------------------------------
coca <- read_csv('../data/tbl/prd/coca_csv.csv')
tbl <- read_excel('../data/tbl/prd/RPT_CultivosIlicitos_2019-06-09--200541.xlsx')
coca <- coca %>% mutate_each(funs(empty_as_na)) 

# Converto factor to numeric
dfm <- map_df(coca[,5:ncol(coca)], as.numeric)
coca <- cbind(coca[,1:4], dfm) %>% 
  as_tibble() %>%  
  gather(year, value, -DEPARTAMENTO, -CODDEPTO, -CODMPIO, -MUNICIPIO) %>% 
  mutate(DEPARTAMENTO = iconv(DEPARTAMENTO, to = 'latin1'),
         MUNICIPIO = iconv(MUNICIPIO, to = 'latin1'))
rm(dfm)

coca %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(suma = sum(value, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  pull(2) %>% 
  mean()


  unique(coca$year)
unique(coca$DEPARTAMENTO) 
  
  
