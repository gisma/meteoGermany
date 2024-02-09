#'
#' Partly the code is taken from
#' Hartmann, K., Krois, J., Waske, B. (2018): E-Learning Project SOGA: Statistics and Geospatial Data Analysis. Department of Earth Sciences, Freie Universitaet Berlin.
#' https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/time-series-analysis/index.html

# Retrieve Federal States by the the getData() function from the raster package
message("::: get masking boundary data :::")
germany <- getData(country = "Germany", level = 1)
bl_sp <- getData(country = "Germany", level = 1)[getData(country = "Germany", level = 1)$NAME_1 == bl, ]

#germany <- geodata::gadm(country="DEU", level=1, path=tempdir())
germany.sf <- st_as_sf(germany)
bl_sf = st_as_sf(bl_sp)
# transform to UTM zone 33
germany.sf <- st_transform(germany.sf, crs = crs)
# dissolve data
states_special = c("Baden-Württemberg","Nordrhein-Westfalen","Hessen","Bayern","Niedersachsen",
                   "Sachsen-Anhalt","Rheinland-Pfalz","Sachsen","Mecklenburg-Vorpommern",
                   "Schleswig-Holstein","Brandenburg","Thüringen","Saarland","Berlin",
                   "Hamburg","Bremen")
DE.states <- germany.sf[germany.sf$NAME_1 %in% states_special,]
DE <- DE.states %>% group_by(NAME_1) %>% summarize()
DE.sp <- as(DE, 'Spatial')

message("::: get srtm elevation data :::")
if (getDEM){
  de_4326 =st_transform(DE,4326)
  download.url <- "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_30x30/TIFF/N30E000.zip"
  zipfile <- paste0(envrmt$path_data_lev0,"/N30E000.zip")
  download.file(download.url,zipfile, mode = "wb")
  unzip(zipfile,exdir = envrmt$path_data_lev0)
  system('gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3035 -of GTiff -cutline /media/creu/742BDA5A2D11BD36/meteoGermany/data/data_lev0/de_4326.shp -cl de_4326 -crop_to_cutline /media/creu/742BDA5A2D11BD36/meteoGermany/data/data_lev0/srtm_germany_dtm.tif /media/creu/742BDA5A2D11BD36/meteoGermany/data/data_lev0/germany.tif')
  system(paste0("gdalwarp -overwrite -s_srs EPSG:4326 -t_srs EPSG:3035 -of GTiff -tr ",res," ", res, " -tap -cutline ",envrmt$path_data_lev0,"/de_4326.shp -cl de_4326 -crop_to_cutline -multi ",envrmt$path_data_lev0,"/cut_n30e000.tif ", envrmt$path_data_lev0,"/germany.tif"))


srtm.germany = raster( paste0(envrmt$path_data_lev0,"/germany.tif"))
# cast to SpatialPixelsDataFrame
srtm.germany.spdf <- as(srtm.germany,
                        'SpatialPixelsDataFrame')
colnames(srtm.germany.spdf@data) <- 'Stationshoehe'

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
srtm500=resample(srtm.germany,template_raster)
srtm500=as.integer(round(srtm500,0))
names(srtm500) <- 'Stationshoehe'

dem = st_as_stars(srtm500)
saveRDS(dem,paste0(envrmt$path_data_lev0,"dem.rds"))
rm(srtm.germany,template_raster,grid.DE,srtm.germany.spdf,germany,DE.sp,srtm500)
}

dem = readRDS(paste0(envrmt$path_data_lev0,"dem.rds"))

######
message("::: get climate data :::")
if (getClimate) {
  # Select daily climate data:
  data("metaIndex")
  m <- metaIndex
  m <- m[m$res=="daily" & m$var=="kl" & (m$per==type) & m$hasfile, ]

  # Transform into spatial object:
  msf <- sf::st_as_sf(m, coords=c("geoLaenge", "geoBreite"), crs=4326)
  # extract station ids for station after startYear
  #lki = msf[as.Date(msf$von_datum,,"%Y-%m-%d") >= as.Date(startJahr,"%Y-%m-%d") ,]$Stations_id
  lki=msf$Stations_id
  rainLK <- pbapply::pblapply(1:length(lki), gemeinden_temp)
  gm_tempMax = do.call(rbind,rainLK)
  #

  # create subset from msf according to the data
  stations = msf[year(msf$von_datum) >=  year(startDate) & year(msf$von_datum) <=  year(endDate) ,]
  names(stations)[1] = "STATIONS_ID"
  merge = merge(stations,gm_tempMax)
  #st_write(merge,paste0(envrmt$path_data_lev0,"/daily_climate.gpkg"))
  
  
  # transform to UTM zone 33
  cVar.sf <- st_transform(merge, crs)
  cVar.sp <- as(cVar.sf, 'Spatial')
  cVar.sf = sf::st_as_sf(cVar.sp)
  saveRDS(merge,paste0(envrmt$path_data_lev0,"/daily_climate_",type,".rds"))
  rm(cVar.sp)
}
cVar.sf = readRDS(paste0(envrmt$path_data_lev0,"/daily_climate_",type,".rds"))

gc()
