
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, sf, velox, doSNOW, foreach, parallel)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen =  999)

rasterOptions(tmpdir = './tst')

# Functions to use --------------------------------------------------------
myFunction <- function(id){
  # id <- ids[1122]
  id <- 88001
  id <- 88564
  print(id)
  sh <- adm[adm@data$ID_ESPACIA %in% id,]
  ls <- raster::crop(lss, sh) %>% 
    raster::mask(., sh)
  ls[which(ls[] > 0)] <- 1
  fr <- raster::crop(frs, sh) %>% 
    raster::mask(., sh)
  fr[which(ls[] > 0)] <- 2
  plot(fr)
  tb <- rasterToPoints(fr) %>% 
    as_tibble() %>% 
    mutate(dpto = iconv(sh@data$NOMBRE_DPT, from = 'UTF-8', to = 'latin1'),
           mpio = iconv(sh@data$NOM_MUNICI, from = 'UTF-8', to = 'latin1'),
           gid = sh@data$ID_ESPACIA) %>% 
    setNames(c('x', 'y', 'value', 'dpto', 'mpio', 'gid')) %>% 
    inner_join(., lbl, by = c('value' = 'value')) %>% 
    group_by(gid, dpto, mpio, class) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(class, count)
  print('Done!')
  rm(sh, ls, fr)
  removeTmpFiles(h = 24)
  saveRDS(object = tb, file = paste0('./rds/loss/loss_', id, '.rds'))
  return(tb)
}

# Load data ---------------------------------------------------------------
lss <- raster('./tif/loss/lossyear_col.tif')
frs <- raster('./tif/treecover/tree_cover2000_col_rcl.tif')
adm <- shapefile('./shp/bse/mpios_geo_ok.shp')
ids <- adm@data$ID_ESPACIA
lbl <- data.frame(value = 0:2,
                  class = c('No_bosque', 'Bosque', 'Deforestacion'))

length(ids)
ids[1122]

# Appy the function -------------------------------------------------------
detectCores()
cl <- makeCluster(15)
registerDoSNOW(cl)
tbl <- foreach(i = 1:length(ids), .packages = c('raster', 'rgdal', 'tidyverse'), .verbose = TRUE) %dopar% {
  myFunction(id = ids[i])
} 
stopCluster(cl)

tbl <- list.files('./rds/loss', full.names = TRUE, pattern = '.rds') %>% 
  map(.x = ., .f = read_rds)
tbl <- bind_rows(tbl)
saveRDS(object = tbl, file = './rds/all_loss.rds')

write.csv(tbl, './deforestation_pixels_ids.csv', row.names = FALSE)



fls <- list.files('./rds/loss', full.names = TRUE, pattern = '.rds')
rsl <- lapply(fls, read_rds)
