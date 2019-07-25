
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, sf, velox, doSNOW, foreach, parallel)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen =  999)

# Functions to use --------------------------------------------------------
myFunction <- function(id){
  # id <- ids[1]
  print(id)
  sh <- shp[shp@data$ID_ESPACIA %in% id,]
  hn <- raster::crop(hns, sh) %>% 
    raster::mask(., sh) 
  dm <- raster::crop(idm, sh) %>% 
    raster::mask(., sh) %>% 
    rasterToPoints() %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', 'value')) %>% 
    filter(value == 1)
  vl <- raster::extract(hn, dm[,1:2])
  dm <- dm %>% mutate(hansen = vl)
  print('Done!')
  return(dm)
}

# Load data ---------------------------------------------------------------
hns <- raster('../data/tif/hsn/prc/treecover/tree_cover2000_col.tif')
shp <- shapefile('../data/shp/bse/mpios_geo.shp')
idm <- raster('../data/tif/ideam/tif/Colombia_Resolucion_Fina_Bosque_NoBosque_2000/Colombia_Resolucion_Fina_Bosque_NoBosque_2000.tif')

# Extract by mask ---------------------------------------------------------
ids <- unique(shp@data$ID_ESPACIA)
id1 <- ids[1:3]

tb1 <- map(.x = id1, .f = myFunction)
tb1 <- bind_rows(tb1)
saveRDS(tb1, '../rds/frs/tb1.rds')

tb2 <- map(.x = id2, .f = myFunction)
tb2 <- bind_rows(tb2)
saveRDS(tb1, '../rds/frs/tb2.rds')

detectCores()
cl <- makeCluster(2)
registerDoSNOW(cl)
tbl <- foreach(i = 1:length(ids), .packages = c('raster', 'rgdal', 'tidyverse'), .verbose = TRUE) %dopar% {
  myFunction(id = ids[i])
} 
stopCluster(cl)

mtx <- matrix(c(0,94,0,94.1,100,1), nrow = 2, byrow = TRUE)
hns_rcl <- reclassify(hns, mtx)
hns_rcl <- raster::crop(hns_rcl, shp) %>% raster::mask(., shp)
writeRaster(hns_rcl, '../data/tif/hsn/prc/treecover/tree_cover2000_col_rcl.tif')

hns
