#'
#' Partly the code is taken from
#' Hartmann, K., Krois, J., Waske, B. (2018): E-Learning Project SOGA: Statistics and Geospatial Data Analysis. Department of Earth Sciences, Freie Universitaet Berlin.
#' https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/time-series-analysis/index.html
#' and more recently from the documentation of rdwd
#' https://bookdown.org/brry/rdwd/use-case-get-all-hourly-rainfall-data-20142016.html
#' 
# Retrieve Federal States by the the getData() function from the raster package
message("::: get masking boundary data :::")
germany <- getData(country = "Germany", level = 1)
bl_sp <- getData(country = "Germany", level = 1)[getData(country = "Germany", level = 1)$NAME_1 == bl, ]

#germany <- geodata::gadm(country="DEU", level=1, path=tempdir())
germany.sf <- st_as_sf(germany)
bl_sf = st_as_sf(bl_sp)
# transform to UTM zone 33
germany.sf <- st_transform(germany.sf, crs = crs)
bl_sf <- st_transform(bl_sf, crs = crs)
# dissolve data
states_special = c("Baden-Württemberg","Nordrhein-Westfalen","Hessen","Bayern","Niedersachsen",
                   "Sachsen-Anhalt","Rheinland-Pfalz","Sachsen","Mecklenburg-Vorpommern",
                   "Schleswig-Holstein","Brandenburg","Thüringen","Saarland","Berlin",
                   "Hamburg","Bremen")
DE.states <- germany.sf[germany.sf$NAME_1 %in% states_special,]
DE <- DE.states %>% group_by(NAME_1) %>% summarize()


message("::: get srtm elevation data :::")
if (downloadDEM){
  de_4326 =st_transform(DE,4326)
  st_write(de_4326,file.path(envrmt$path_data_lev0,"/de_4326.shp"))
  
  download.url <- "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_30x30/TIFF/N30E000.zip"
  zipfile <- paste0(envrmt$path_data_lev0,"/N30E000.zip")
  download.url = "https://opendem.info/downloads/srtm_germany_dtm.zip"
  zipfile <- paste0(envrmt$path_data_lev0,"/srtm_germany_dtm.zip")
  download.file(download.url,zipfile, mode = "wb")
  unzip(zipfile,exdir = envrmt$path_data_lev0)
  #system('gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3035 -of GTiff -cutline /media/creu/742BDA5A2D11BD36/meteoGermany/data/data_lev0/de_4326.shp -cl de_4326 -crop_to_cutline /media/creu/742BDA5A2D11BD36/meteoGermany/data/data_lev0/srtm_germany_dtm.tif /media/creu/742BDA5A2D11BD36/meteoGermany/data/data_lev0/germany.tif')
  #system(paste0("gdalwarp -overwrite -s_srs EPSG:4326 -t_srs EPSG:4326 -of GTiff -tr ",res," ", res, " -tap -cutline ",envrmt$path_data_lev0,"/de_4326.shp -cl de_4326 -crop_to_cutline -multi ",envrmt$path_data_lev0,"/srtm_germany_dtm.tif ", envrmt$path_data_lev0,"/germany.tif"))
  

  srtm.germany = terra::mask(terra::rast(file.path(envrmt$path_data_lev0,"/srtm_germany_dtm.tif ")),de_4326)
  # # cast to SpatialPixelsDataFrame
  # srtm.germany.spdf <- as(srtm.germany,
  #                         'SpatialPixelsDataFrame')
  # colnames(srtm.germany.spdf@data) <- 'Stationshoehe'
  # 
  message("::: create template raster :::")
  # create template raster
  grid.DE <- expand.grid(x = seq(from = round(st_bbox(germany.sf)["xmin"]),
                                 to = round(st_bbox(germany.sf)["xmax"]),
                                 by = res),
                         y = seq(from = round(st_bbox(germany.sf)["ymin"]),
                                 to = round(st_bbox(germany.sf)["ymax"]),
                                 by = res))
  coordinates(grid.DE) <- ~x + y
  crs(grid.DE)=crs
  
  # raster
  template_raster <-grid.DE %>%
    raster::rasterFromXYZ(
      crs = crs)
  srtm.germany = terra::project(srtm.germany,crs(template_raster))
  srtm500=terra::resample(srtm.germany,rast(template_raster))
  srtm500=round(srtm500,0)
  names(srtm500) <- 'Stationshoehe'
  
  dem = st_as_stars(srtm500)
  saveRDS(dem,paste0(envrmt$path_data_lev0,"dem.rds"))
  rm(srtm.germany,template_raster,grid.DE,srtm.germany.spdf,germany,DE.sp,srtm500)
}

dem = readRDS(paste0(envrmt$path_data_lev0,"dem.rds"))


gc()
