#' @title Extraction of Climate Raster Statistics by Community or Federal State
#'
#' @description
#' Performs zonal statistics on daily interpolated climate raster data per administrative unit
#' (either municipalities or federal states). Includes:
#' \itemize{
#'   \item Classification of physically implausible values (value capping)
#'   \item Efficient extraction of descriptive statistics using \code{exactextractr}
#'   \item Optional masking and clipping of raster data to federal states
#'   \item Export of individual \code{.csv} statistics per day and merged outputs
#' }
#'
#' Raster values are processed for a given climate variable (e.g., temperature, precipitation).
#' The workflow supports both municipality-level and state-level statistics, controlled via
#' \code{calc_commu} and \code{calc_bl}.
#'
#' @section Statistical Outputs:
#' For each polygon (community or state), the following are computed:
#' \itemize{
#'   \item Minimum, maximum, and majority class
#'   \item Median and specified quantiles
#'   \item Minority class and pixel count
#'   \item Variance, standard deviation, and coefficient of variation
#' }
#'
#' @section Classification Logic:
#' Value ranges are clipped per variable to filter invalid sensor values or model artifacts.
#' Custom classification matrices are applied based on the variable name.
#'
#' @param cVar [character] name of the climate variable (e.g., "TMK", "RSK")
#' @param clim_files [character] vector of full paths to input raster files (*.tif)
#' @param envrmt [list] project environment list with standardized paths
#' @param calc_commu [logical] whether to compute municipality-level stats
#' @param calc_bl [logical] whether to compute federal state (BL) stats
#' @param bl [character] short name of Bundesland (e.g., "Hessen")
#' @param gemeinden_sf_3035 [sf] polygon layer of all municipalities in EPSG:3035
#' @param bl_sf [sf] polygon layer of one federal state (if \code{calc_bl = TRUE})
#' @param common_quantiles [numeric] quantiles used for descriptive statistics
#' @param dig [integer] numeric rounding precision for each variable
#'
#' @return
#' Creates daily `.csv` files per administrative unit containing statistical summaries.
#' Also exports clipped raster files for BL-level analysis and concatenated `.out` tables
#' across all days.
#'
#' @details
#' The raster classification step corrects implausible values such as negative precipitation
#' or extreme temperatures. The use of `exactextractr` ensures high performance for complex
#' polygon boundaries and partial raster overlap.
#'
#' @seealso \code{\link[exactextractr]{exact_extract}}, \code{\link[terra]{classify}},
#' \code{\link[data.table]{fwrite}}, \code{\link[sjmisc]{replace_columns}}
#'
#' @author
#' Chris Reudenbach, \email{creuden@@gmail.com}


# ---- Load Gemeinde geometries ----
gemeinden_sf_3035 = st_read(paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.gpkg"))

# ---- Prepare output directories ----
clim_files <- sort(list.files(paste0(envrmt$path_data_lev1,"/", cVar), paste0("*", cVar, "\\.tif$"), full.names = TRUE))

if (!dir.exists(file.path(envrmt$path_data_lev2, cVar))) dir.create(file.path(envrmt$path_data_lev2, cVar), recursive = TRUE)
if (calc_bl & !dir.exists(file.path(envrmt$path_data_lev1, bl, cVar))) dir.create(file.path(envrmt$path_data_lev1, bl, cVar), recursive = TRUE)
if (calc_bl & !dir.exists(file.path(envrmt$path_data_lev2, bl, cVar))) dir.create(file.path(envrmt$path_data_lev2, bl, cVar), recursive = TRUE)

# ---- Define reusable classification logic ----
classify_raster_by_cVar <- function(cVar, rast) {
  ranges <- list(
    PM  = c(0, 954.9, 954.9, 1060.6, 99999, 1060.6),
    UPM = c(-999, 0, 0, 100, 99999, 100),
    TXK = c(-999, -46, -46, 42, 99999, 42),
    TMK = c(-999, -46, -46, 42, 99999, 42),
    TNK = c(-999, -46, -46, 42, 99999, 42),
    NM  = c(-1000, 0, 0, 8, 99999, 8),
    RSK = c(-1000, 0, 0, 312, 99999, 312)
  )
  if (cVar %in% names(ranges)) {
    m <- matrix(ranges[[cVar]], ncol = 3, byrow = TRUE)
    return(terra::classify(rast, m, include.lowest = TRUE))
  } else {
    return(rast)
  }
}

# ---- Define quantiles once ----
common_quantiles <- c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.25, 0.5, 0.75, 0.9)

# ---- CSV export helper ----
write_stat_csv <- function(sf_df, file) {
  df <- st_drop_geometry(sf_df)
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(df[numeric_cols], round, digits = dig)
  var_fin <- sjmisc::replace_columns(sf_df, df)
  data.table::fwrite(st_drop_geometry(var_fin), file = file, dec = ".")
}

# ---- Loop over climate raster files ----
matrix_of_sums <- parallel::mclapply(seq_along(clim_files), function(i) {
  
  outfile_commu <- file.path(envrmt$path_data_lev2, cVar, paste0(tools::file_path_sans_ext(basename(clim_files[i])), ".csv"))
  if (file.exists(outfile_commu)) return(NULL)
  
  if (cVar == "SDK") {
    correct_daytime(fn = clim_files[i])
    dig <- 3
    return(NULL)
  }
  
  current <- terra::rast(clim_files[i])
  current <- classify_raster_by_cVar(cVar, current)
  names(current) <- xfun::sans_ext(basename(clim_files[i]))
  dig <- ifelse(cVar %in% c("PM", "TXK", "TMK", "TNK", "NM", "RSK"), 1, 0)
  
  if (calc_bl) {
    stat_vars <- cbind(
      bl_sf,
      exactextractr::exact_extract(current, bl_sf,
                                   c("min", "max", "count", "majority", "median", "quantile", "minority", "variance", "stdev", "coefficient_of_variation"),
                                   quantiles = common_quantiles
      )
    )
    stat_vars$date <- substr(tools::file_path_sans_ext(basename(clim_files[i])), 1, 10)
    
    out_bl_file <- file.path(envrmt$path_data_lev2, bl, cVar, paste0(tools::file_path_sans_ext(basename(clim_files[i])), ".csv"))
    write_stat_csv(stat_vars, out_bl_file)
    
    masked <- terra::mask(current, bl_sf)
    cropped <- terra::crop(masked, bl_sf)
    raster::writeRaster(cropped, file.path(envrmt$path_data_lev1, bl, cVar, paste0(bl, basename(clim_files[i]))), overwrite = TRUE)
  }
  
  if (calc_commu) {
    stat_vars <- cbind(
      gemeinden_sf_3035,
      exactextractr::exact_extract(current, gemeinden_sf_3035,
                                   c("min", "max", "count", "majority", "median", "quantile", "minority", "variance", "stdev", "coefficient_of_variation"),
                                   quantiles = common_quantiles
      )
    )
    stat_vars$date <- substr(tools::file_path_sans_ext(basename(clim_files[i])), 1, 10)
    
    write_stat_csv(stat_vars, outfile_commu)
  }
  
}, mc.cores = 16, mc.allow.recursive = TRUE)

# ---- Merge CSVs ----
if (calc_bl) {
  system(paste0("head -n 1 ", envrmt$path_data_lev2, "/", bl, cVar, "/2003-01-01_", cVar, ".csv > ", envrmt$path_data_lev2, "/", bl, cVar, "/", bl, cVar, "_2003-2021.out && tail -n+2 -q ", envrmt$path_data_lev2, "/", bl, cVar, "/*", cVar, ".csv >> ", envrmt$path_data_lev2, "/", bl, cVar, "/", bl, cVar, "_2003-2021.out"), intern = FALSE)
}

if (calc_commu) {
  system(paste0("head -n 1 ", envrmt$path_data_lev2, "/", cVar, "/2003-01-01_", cVar, ".csv > ", envrmt$path_data_lev2, "/", cVar, "/", cVar, "_2003-2021.out && tail -n+2 -q ", envrmt$path_data_lev2, "/", cVar, "/*", cVar, ".csv >> ", envrmt$path_data_lev2, "/", cVar, "/", cVar, "_2003-2021.out"), intern = FALSE)
}
