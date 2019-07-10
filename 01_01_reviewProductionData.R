
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, readxl, raster, rgdal, sf, rgeos, velox)

# Initial setup -----------------------------------------------------------
g <- gc(reset = T)
rm(list = ls())
options(sipen = 999)

# Functions to use --------------------------------------------------------
empty_as_na <- function(x){
  if('factor' %in% class(x)) x <- as.character(x) 
  ifelse(as.character(x)!='', x, NA)
}

# Load base shapefile -----------------------------------------------------
adm <- shapefile('../data/_shp/_bse/mpios_geo.shp')
adm@data$NOM_MUNICI <- iconv(adm@data$NOM_MUNICI, from = 'UTF-8', to = 'latin1')
adm@data$NOMBRE_DPT <- iconv(adm@data$NOMBRE_DPT, from = 'UTF-8', to = 'latin1')

duplicated(adm$ID_ESPACIA)

dup <- adm@data[duplicated(adm@data$ID_ESPACIA),] 
sft <- st_as_sf(adm)
sft <- sft[!duplicated(sft$ID_ESPACIA),] 
st_write(obj = sft, dsn = '../_data/_shp/_bse', layer = 'mpios_geo_ok', driver = 'ESRI Shapefile', update = TRUE)
rm(adm, dup)


# Load data cacao ---------------------------------------------------------
cacao <- read_csv('../data/_tbl/_prd/production_tidy_all.csv')
yrs_cacao <- unique(cacao$PERIODO)
cacao <- cacao %>% filter(PERIODO %in% 2007:2017)
smm_cacao <- cacao %>% 
  group_by(COD_MUNI, DPTO, MPIO) %>%
  summarize(crp = mean(AREA_SEMBRADA_HA, na.rm = TRUE),
            hrv = mean(AREA_COSECHADA_HA, na.rm = TRUE),
            prd = mean(PRODUCCION, na.rm = TRUE),
            rdt = mean(RDTOS, na.rm = TRUE)) %>%
  ungroup()

n_cacao <- cacao %>% 
  dplyr::group_by(DPTO, PERIODO) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup()
unique(n_cacao$DPTO)

cacao %>% 
  distinct(PERIODO, DPTO) %>% 
  group_by(PERIODO) %>% 
  summarise(count = n()) %>% 
  ungroup()

p07 <- cacao %>% 
  filter(PERIODO == 2007) %>% 
  pull(DPTO) %>% 
  unique() %>% 
  iconv(., to = 'latin1')
p17 <- cacao %>% 
  filter(PERIODO == 2017) %>% 
  pull(DPTO) %>% 
  unique() %>% 
  iconv(., to = 'latin1')
setdiff(p17, p07)

n_year <- cacao %>% 
  # dplyr::select(-MPIO, COD_MUNI) %>% 
  dplyr::group_by(PERIODO) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup()



# Load data coca ----------------------------------------------------------
coca <- read.csv('../data/_tbl/_prd/coca_csv.csv') %>% as_tibble()
coca <- coca %>% mutate_each(funs(empty_as_na)) 

# Converto factor to numeric
dfm <- map_df(coca[,5:ncol(coca)], as.numeric)
coca <- cbind(coca[,1:4], dfm) %>% as_tibble(); rm(dfm)
coca <- coca %>% 
  gather(var, value, -DEPARTAMENTO, -CODDEPTO, -CODMPIO, -MUNICIPIO) %>% 
  mutate(year = str_sub(var, 2, 5) %>% as.numeric()) %>% 
  dplyr::select(-var) 
yrs_coca <- unique(coca$year)

# Summarize
smm_coca <- coca %>% 
  group_by(CODMPIO, DEPARTAMENTO, MUNICIPIO) %>% 
  summarize(area_coca = mean(value, na.rm = TRUE)) %>% 
  ungroup()


coca %>% 
  group_by(year) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  ungroup()



# Departaments coca
dpt_coca <- smm_coca %>%
  group_by(DEPARTAMENTO) %>%
  summarize(area_coca = sum(area_coca)) %>% 
  ungroup()

sum(smm_coca$area_coca, na.rm = T)
sum(dpt_coca$area_coca, na.rm = T)

# Join between cacao and coca ---------------------------------------------
smm_coca[duplicated(smm_coca$CODMPIO),]
smm_cacao[duplicated(smm_cacao$COD_MUNI),] # There isn't duplicated values

nrow(smm_coca)
nrow(smm_cacao)

sft <- sft %>% 
  dplyr::select(COD_DEPTO, ID_ESPACIA, NOMBRE_DPT, NOM_MUNICI) %>% 
  mutate(ID_ESPACIA = as.numeric(as.character(ID_ESPACIA)))
join <- full_join(sft, smm_cacao %>% dplyr::select(-DPTO, -MPIO), by = c('ID_ESPACIA' = 'COD_MUNI'))
join <- full_join(join, smm_coca %>% dplyr::select(-DEPARTAMENTO, -MUNICIPIO), by = c('ID_ESPACIA' = 'CODMPIO'))

st_write(obj = join, dsn = '../_data/_shp/_prd', layer = 'cacao_coca_207_17', driver = 'ESRI Shapefile', update = TRUE)



dpt_coca %>% top_n(wt = area_coca, n = 5)

# New analysis to get the municipalities
all <- st_read('../_data/_shp/_prd/cacao_coca_207_17.shp')
coca <- all %>% as.data.frame %>% dplyr::select(ID_ESPACIA, NOMBRE_DPT, NOM_MUNICI, area_coca) %>% as_tibble() %>% na.omit()
all %>% top_n(wt = area_coca, n = 5)
coca %>% top_n(wt = area_coca, n = 5)

# Join production coca, cacao and suitability cacao
stb <- shapefile('../_data/_shp/_stb/stb_prc_alta.shp')
stb <- stb %>% as_data_frame() %>% dplyr::select(ID_ESPACIA, sum_n, prc)
al2 <- inner_join(stb, all, by = 'ID_ESPACIA')

dpt_al2 <- al2 %>% 
  group_by(NOMBRE_DPT) %>%
  summarize(crp = sum(crp, na.rm = TRUE),
            area_coca = sum(area_coca, na.rm = TRUE),
            sum_n = sum(sum_n, na.rm = TRUE))

dpt_al2 <- dpt_al2 %>% dplyr::select(-sum_n)

dpt_al2 %>%
  top_n(wt = area_coca, n = 5) %>% 
  arrange(desc(area_coca))


fil <- al2 %>% 
  dplyr::select(ID_ESPACIA, sum_n, prc, NOMBRE_DPT, NOM_MUNICI, crp, hrv, area_coca) %>%
  filter(prc > 66.99, crp > 356, area_coca > 142.) %>% 
  dplyr::select(NOMBRE_DPT, NOM_MUNICI, prc, crp, area_coca)

as.character(unique(fil$NOMBRE_DPT))


