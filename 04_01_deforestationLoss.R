
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, sf, velox, doSNOW, foreach, parallel)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen =  999)

rasterOptions(tmpdir = '../tst')

# Functions to use --------------------------------------------------------
myFunction <- function(id){
  # id <- ids[1]
  print(id)
  sh <- adm[adm@data$ID_ESPACIA %in% id,]
  ls <- raster::crop(lss, sh) %>% 
    raster::mask(., sh)
  fr <- raster::crop(frs, sh) %>% 
    raster::mask(., sh)
  fr[which(ls[] > 0)] <- 2
  tb <- rasterToPoints(fr) %>% 
    as_tibble() %>% 
    mutate(dpto = iconv(sh@data$NOMBRE_DPT, from = 'UTF-8', to = 'latin1'),
           mpio = iconv(sh@data$NOM_MUNICI, from = 'UTF-8', to = 'latin1')) %>% 
    setNames(c('x', 'y', 'value', 'dpto', 'mpio')) %>% 
    inner_join(., lbl, by = c('value' = 'value'))
  print('Done!')
  rm(sh, ls, fr)
  removeTmpFiles(h = 24)
  return(tb)
}

# Load data ---------------------------------------------------------------
lss <- raster('../data/tif/hsn/prc/lossyear/lossyear_col.tif')
frs <- raster('../data/tif/hsn/prc/treecover/tree_cover2000_col_rcl.tif')
adm <- shapefile('../data/shp/bse/mpios_geo_ok.shp')
ids <- adm@data$ID_ESPACIA
lbl <- data.frame(value = 1:3,
                  class = c('Bosque', 'No bosque', 'Deforestacion'))
# Appy the function -------------------------------------------------------
detectCores()
cl <- makeCluster(4)
registerDoSNOW(cl)
tbl <- foreach(i = 1:length(ids), .packages = c('raster', 'rgdal', 'tidyverse'), .verbose = TRUE) %dopar% {
  myFunction(id = ids[i])
} 
stopCluster(cl)
rsl <- map(.x = ids, .f = myFunction)




