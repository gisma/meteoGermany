#' --- Main Climate Interpolation Control Script ---
#'
#' Spatial interpolation of climate variables using Kriging with covariates
#'
#' @description
#' This control script performs spatial interpolation of gridded climate variables
#' using Kriging with external drift (KED). The script
#' supports DEM-based covariates and applies robust modeling strategies via the 
#' `gstat` package. This method is suitable for downscaling coarse climate data
#' or filling gaps in station-based datasets.
#'
#' @details
#' The script includes:
#' \itemize{
#'   \item Preprocessing and merging of point-based climate data (e.g., DWD stations)
#'   \item Integration of a DEM as a topographic covariate
#'   \item Calculation of variogram models
#'   \item Spatial interpolation using Kriging with External Drift (KED): suitable if the external covariate acts as a spatial trend (e.g., elevation for temperature)
#'   \item Output of gridded interpolations for further modeling (e.g., vegetation, microclimate)
#' }
#' 
#' Credits:
#' Partly adapted from:
#' - Hartmann, K., Krois, J., Waske, B. (2018): E-Learning Project SOGA, FU Berlin
#'   https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/time-series-analysis/index.html
#' - Documentation of `rdwd`: https://bookdown.org/brry/rdwd/
#'
#'
#' @references
#' Hengl, T., Heuvelink, G.B.M., & Stein, A. (2003). 
#' \emph{Comparison of kriging with external drift and regression-kriging}. 
#' ITC technical note. Available online at \url{https://ris.utwente.nl/ws/portalfiles/portal/448469781/hengl_comparison.pdf}
#'
#' Pebesma, E.J. (2006). 
#' \emph{The gstat package}. Computers & Geosciences, 30(7), 683–691. 
#' \doi{10.1016/j.cageo.2004.03.012}
#'
#' Hofstra, N., Haylock, M., New, M., & Jones, P.D. (2009). 
#' \emph{Testing E-OBS European high-resolution gridded data set of daily precipitation and surface temperature}. 
#' J. Geophys. Res., 114, D21101. \doi{10.1029/2009JD011799}
#' 
#' Dolinar, M. (2006), 
#' \emph{Spatial interpolation of sunshine duration in Slovenia}. Met. Apps, 13: 375-384. 
#' \doi{10.1017/S1350482706002362}
#'
#' @author
#' Chris Reudenbach <creuden@gmail.com>
#'
#' @keywords spatial interpolation kriging external drift regression kriging DEM climate modeling
#'
#' @seealso \code{\link[gstat]{krige}}, \code{\link[gstat]{variogram}}, \code{\link[terra]{predict}}
#'
#'
#' @author Chris Reudenbach

# ---- Setup project environment ----
library(envimaR)
library(rprojroot)


# Define custom subfolders for climate data
appendProjectDirList = c(
  "data/data_lev0/CDC_KL",
  "data/data_lev0/GhcnDaily",
  "data/data_lev0/GhcnMonthly"
)

# Find project root (e.g., from Quarto or RStudio project)
root_folder = find_rstudio_root_file()

# Load setup script that defines envrmt list and paths
source(file.path(root_folder, "src/functions/000_setup.R"))

# ---- Default parameters ----
crs   = raster::crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
epsg  = 3035
res   = 500             # grid resolution for the prediction DEM
startDate = "2010-01-01"
endDate   = "2024-12-31"
minStations = 150
type      = "historical"   # or "recent" (last ~500 days)
var       = "kl"           # DWD climate variable group
reso      = "daily"
downloadDEM = FALSE        # set TRUE only once
getClimate  = TRUE         # download new climate data
#minStations = 15           # minimum number of valid stations for kriging
calc_commu  = TRUE         # extract results per municipality
calc_bl     = FALSE        # optionally calculate one state only
bl          = "Hessen"     # name of state
PM          = FALSE        # specific handling for air pressure
param       = "RSK"        # one of: c("RSK", "SDK", "NM", "UPM", "TXK", "TNK", "TMK", "TGK", "VPM", "PM ")

# ---- Prepare auxiliary data ----
if (downloadDEM) {
  dem_DEU <- prepare_DEM(envrmt = envrmt, bl = "DEU")
}
dem <- readRDS(file.path(envrmt$path_data_lev0, "dem.rds"))
dem = stars::st_warp(dem,crs=epsg)

# ---- Climate variable loop (main prediction) ----
for (cVar in c("RSK", "SDK", "NM", "UPM", "TXK", "TNK", "TMK", "TGK", "VPM", "PM ")) {
  
  # Use one single parameter for now (override cVar)
  cVar = param
  
  # Download filtered and preprocessed daily climate data
  cVar.sf = ex_clim_new(
    startDate = startDate,
    endDate   = endDate,
    reso      = reso,
    var       = var,
    type      = type,
    param     = param
  )
  
  # Extract all unique available dates
  dat_list = sort(unique(as.character(cVar.sf$MESS_DATUM)))
  
  # ---- Loop over all dates (parallel with progress bar) ----
  # ---- Loop over all dates (parallel with progress bar) ----
  matrix_of_sums <- pbmcapply::pbmclapply(seq_along(dat_list), function(n) {
    currentDate <- dat_list[n]
    
    # Skip dates outside the target interval
    if (as.Date(currentDate) < as.Date(startDate) | as.Date(currentDate) > as.Date(endDate)) {
      return(NULL)
    }
    
    cd <- substr(currentDate, 1, 10)
    target_dir  <- file.path(envrmt$path_data_lev1, cVar)
    target_file <- file.path(target_dir, paste0(cd, "_", cVar, ".tif"))
    
    # ---- Skip if file already exists ----
    if (file.exists(target_file)) {
      message("Skipping existing file: ", target_file)
      return(NULL)
    }
    
    # Filter values for the current day
    cVar.sf.day <- cVar.sf[as.character(cVar.sf$MESS_DATUM) == as.Date(currentDate), ]
    
    # Apply unit corrections and value range checks
    dat <- sanitize_climate_param(cVar.sf.day, cVar, date = currentDate)
    
    # Geometry column fix
    geom_col <- intersect(c("geometry", "geom"), names(dat))
    if (length(geom_col) == 1 && geom_col != "geometry") {
      names(dat)[names(dat) == geom_col] <- "geometry"
    }
    st_geometry(dat) <- "geometry"
    dat <- dat[, c("Stationshoehe", "tmp", "geometry")]
    
    dat$tmp <- as.numeric(dat$tmp)
    names(dat)[names(dat) == "tmp"] <- cVar
    data <- dat %>% drop_na()
    
    # Ensure CRS matches DEM
    if (sf::st_crs(data) != sf::st_crs(dem)) {
      data <- sf::st_transform(data, crs = epsg)
    }
    
    # Proceed only if enough valid stations
    if (sum(!is.na(data[[cVar]])) > minStations) {
      data <- dplyr::distinct(data, geometry, .keep_all = TRUE)
      data <- st_transform(data, st_crs(dem))
      
      vm.auto <- automap::autofitVariogram(as.formula(paste(cVar, "~1")), input_data = data)
      
      pred <- gstat::krige(
        formula = as.formula(paste(cVar, "~Stationshoehe")),
        locations = data,
        newdata = dem,
        model = vm.auto$var_model,
        debug.level = -1
      )
      
      if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
      if (is.null(envrmt[[paste0("path_", cVar)]])) {
        envrmt[[paste0("path_", cVar)]] <- target_dir
      }
      
      stars::write_stars(pred, target_file, overwrite = TRUE, options = "COMPRESS=LZW")
      rm(pred); gc()
    } else {
      stars::write_stars(dem * 0 - 9999, target_file, overwrite = TRUE, options = "COMPRESS=LZW")
    }
    
    message("Written: ", target_file)
    return(target_file)
  }, mc.cores = 12, mc.allow.recursive = TRUE)
  
# ---- Final step: extract to municipality level ----
# (Assumes community-level processing script is modular)
for (cVar in c("RSK", "SDK", "NM", "UPM", "TXK", "TNK", "TMK", "TGK", "VPM", "PM ")) {
  #source(file.path(root_folder, "src/main_script_calculate_communities.R"))
  extract_climate_statistics(
    cVar = "TXK",
    envrmt = envrmt,
    calc_commu = TRUE,
    gemeinden_sf_3035 = st_read(file.path(envrmt$path_data_lev0, "gemeinden_DE_3035.gpkg")),
    dig = 1
  )
}
