#' ---
#' @title Download and Prepare Elevation Data and Administrative Boundaries for Germany
#' 
#' @description
#' This script downloads and processes boundary and elevation data (DEM) for Germany.
#' It uses `geodata`, `terra`, and optionally SRTM data (resampled to a fixed grid).
#' This preprocessing step is essential for later use as a covariate in spatial interpolation methods 
#' such as kriging with external drift or regression-kriging.
#'
#' Data sources:
#' - Administrative boundaries: [GADM via geodata](https://github.com/rspatial/geodata)
#' - Elevation (SRTM): [opendem.info](https://opendem.info)
#'
#' @author
#' Chris Reudenbach
#' @keywords
#' DEM, elevation, preprocessing, kriging, spatial covariates, SRTM, GADM, raster
#' @export
#' ---

message("::: get masking boundary data :::")

library(geodata)
library(terra)

# Load administrative boundaries of Germany (level 1 = federal states)
germany <- geodata::gadm(country = "DEU", level = 1, path = tempdir())
bl_sp <- germany[germany_states$NAME_1 == bl, ]

# Convert to sf objects
germany.sf <- st_as_sf(germany)
bl_sf <- st_as_sf(bl_sp)

# Transform to desired CRS
germany.sf <- st_transform(germany.sf, crs = crs)
bl_sf <- st_transform(bl_sf, crs = crs)

# Dissolve boundary polygons to union by state name
states_special = c("Baden-Württemberg","Nordrhein-Westfalen","Hessen","Bayern",
                   "Niedersachsen","Sachsen-Anhalt","Rheinland-Pfalz","Sachsen",
                   "Mecklenburg-Vorpommern","Schleswig-Holstein","Brandenburg",
                   "Thüringen","Saarland","Berlin","Hamburg","Bremen")

DE.states <- germany.sf[germany.sf$NAME_1 %in% states_special,]
DE <- DE.states %>% group_by(NAME_1) %>% summarize()

message("::: get srtm elevation data :::")

if (downloadDEM){
  # Convert to WGS84 for SRTM coverage
  de_4326 = st_transform(DE, 4326)
  st_write(de_4326, file.path(envrmt$path_data_lev0, "de_4326.shp"))
  
  # SRTM download link
  download.url = "https://opendem.info/downloads/srtm_germany_dtm.zip"
  zipfile <- file.path(envrmt$path_data_lev0, "srtm_germany_dtm.zip")
  
  # Download and unzip
  download.file(download.url, zipfile, mode = "wb")
  unzip(zipfile, exdir = envrmt$path_data_lev0)
  
  # Load and mask SRTM with administrative boundary
  srtm.germany = terra::mask(
    terra::rast(file.path(envrmt$path_data_lev0, "srtm_germany_dtm.tif")),
    de_4326
  )
  
  message("::: create template raster :::")
  
  # Build a regular grid raster using bounding box of Germany
  grid.DE <- expand.grid(
    x = seq(from = round(st_bbox(germany.sf)["xmin"]),
            to = round(st_bbox(germany.sf)["xmax"]),
            by = res),
    y = seq(from = round(st_bbox(germany.sf)["ymin"]),
            to = round(st_bbox(germany.sf)["ymax"]),
            by = res)
  )
  coordinates(grid.DE) <- ~x + y
  crs(grid.DE) <- crs
  
  # Create raster template from grid
  template_raster <- raster::rasterFromXYZ(grid.DE, crs = crs)
  
  # Project and resample SRTM to target grid and resolution
  srtm.germany <- terra::project(srtm.germany, crs(template_raster))
  srtm500 <- terra::resample(srtm.germany, rast(template_raster))
  srtm500 <- round(srtm500, 0)
  
  names(srtm500) <- "Stationshoehe"
  
  # Convert to stars for later use in interpolation
  dem <- st_as_stars(srtm500)
  
  # Save to RDS file for downstream processing
  saveRDS(dem, file.path(envrmt$path_data_lev0, "dem.rds"))
  
  # Cleanup
  rm(srtm.germany, template_raster, grid.DE, srtm500)
}

# Load previously saved DEM if not downloading
dem <- readRDS(file.path(envrmt$path_data_lev0, "dem.rds"))

gc()
