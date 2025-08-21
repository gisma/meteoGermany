# get_cl_data <- function(l)
# {
# 
#   urls <- selectDWD(id = lki[l], res="daily", var="kl", per=type)#, outvec=TRUE)
#   clims <- dataDWD(urls, varnames=FALSE, dir=envrmt$path_GhcnDaily)
#   climdata <- clims[c("STATIONS_ID","MESS_DATUM","RSK","SDK","NM","VPM","PM","TMK","UPM","TXK","TNK","TGK")]
#   return(climdata)
# 
# }
# 
# get_climdata <- function(l,var="pressure",res ="hourly",col="P")
# {
#   
#   urls <- selectDWD(id = lki[l], res=res, var=var, per=type)#, outvec=TRUE)
#   clims <- dataDWD(urls, varnames=FALSE, dir=envrmt$path_GhcnDaily)
#   climdata <- clims[c("STATIONS_ID","MESS_DATUM",col)]
#   return(climdata)
#   
# }

#'
#' Partly the code is taken from
#' https://bookdown.org/brry/rdwd/use-case-get-all-hourly-rainfall-data-20142016.html
#' 
read_cl_param <- function(file, fread=TRUE,sd=NULL,ed = NULL, par = NULL, ...)
{
  climdata <- readDWD(file, fread=fread)
  climdata <- climdata[climdata$MESS_DATUM > as.POSIXct(as.Date(sd)) & 
                         climdata$MESS_DATUM < as.POSIXct(as.Date(ed))    , ]
  climdata <- climdata[ , c("STATIONS_ID","MESS_DATUM", par)]
  climdata$MESS_DATUM <- as.Date(climdata$MESS_DATUM) # might save some memory space...
  return(climdata)
}
#'
#' Partly the code is taken from
#' https://bookdown.org/brry/rdwd/use-case-get-all-hourly-rainfall-data-20142016.html
#' 
#' 
#' 
ex_clim = function(startDate=NULL,endDate=NULL,reso=NULL,var=NULL,type=NULL,param=NULL){
  message("::: get climate data :::")
  # Select daily climate data:
  data("metaIndex")
  myIndex <- metaIndex[
    metaIndex$von_datum < as.Date(startDate) &
      metaIndex$bis_datum > as.Date(endDate) & metaIndex$hasfile   ,  ]
  data("fileIndex")
  links <- fileIndex[
    suppressWarnings(as.numeric(fileIndex$id)) %in% myIndex$Stations_id &
      fileIndex$res==reso &
      fileIndex$var==var &
      fileIndex$per==type         , "path" ]
  localfiles <- dataDWD(
    links,
    joinbf = TRUE,
    sleep = 0.2,
    read = FALSE,
    dir = envrmt$path_CDC_KL
  )
  localfiles <- dataDWD(links, joinbf=TRUE, sleep=0.2, read=FALSE,dir=envrmt$path_CDC_KL)
  localfiles = localfiles[file.exists(localfiles)]
  
  # extract station ids 
  matrix_of_params <- parallel::mclapply( seq_along(localfiles), function(n){
    #for (n in seq_along(1:3)){
    read_cl_param(localfiles[n],sd = startDate,ed=endDate,par = param,)
  }, mc.cores = 16, mc.allow.recursive = TRUE)
  var_all = data.table::rbindlist(matrix_of_params) 
  
  # Transform into spatial object:
  msf <- sf::st_as_sf(myIndex, coords=c("geoLaenge", "geoBreite"), crs=4326)
  stations = msf[msf$Stations_id %in%  unique(var_all$STATIONS_ID),]
  stations = stations[stations$res == reso & stations$var==var & stations$per == type & stations$hasfile==TRUE   , ]
  names(stations)[1] = "STATIONS_ID"
  
  merge_cl = merge(stations,var_all)
  # transform to crs
  cVar.sf <- st_transform(merge_cl, crs)
  saveRDS(cVar.sf,paste0(envrmt$path_data_lev0,"/daily_climate_",type,"_",param,".rds"))  

  gc()
  
  return(cVar.sf )
  # actually this means to extract hourly data in this case PM 
  # it is hard wired so far, have a look at get_climdata
  #   if (PM) {
  #     pressureLK <- pbapply::pblapply(1:length(lki), get_climdata)
  #     pressureLK_all = do.call(rbind,pressureLK)
  #     merge_PM = merge(stations,pressureLK_all)
  #     cVar_PM.sf <- st_transform(merge_PM, crs)
  #     saveRDS(cVar_PM.sf,paste0(envrmt$path_data_lev0,"/hourly_PM_",type,".rds"))
  #      # calculate daily mean from hourly data  
  #     cVar_PM.sf$Date <- as.Date(cVar_PM.sf$MESS_DATUM, format = "%Y%m%d%H")
  #     # Group by STATIONS_ID and Date, then calculate mean of P
  #     cVar_PM.sf <- cVar_PM.sf %>%
  #       group_by(STATIONS_ID,Stationshoehe,Date) %>%
  #       summarise(PM = mean(P))
  #     names(cVar_PM.sf) = c("STATIONS_ID", "Stationshoehe", "MESS_DATUM", "PM","geometry")
  #     saveRDS(daily_means,paste0(envrmt$path_data_lev0,"/daily_PM_",type,".rds"))
  #     
  # }
}


#' Modern DWD Climate Data Extractor (robuste Version mit Cache, Filter und Export)
#'
#' @param startDate Startdatum (z. B. "2010-01-01")
#' @param endDate Enddatum (z. B. "2020-12-31")
#' @param reso Auflösung (z. B. "daily")
#' @param var DWD Variable (z. B. "kl")
#' @param type Zeitraumtyp (z. B. "historical")
#' @param param Zielparameter (z. B. "TMK")
#' @return sf-Objekt mit Messwerten
#' @export
ex_clim_new <- function(startDate, endDate, reso = "daily", var = "kl", type = "historical", param = "TMK") {

  
  message("::: Suche verfügbare DWD-Stationen :::")
  outname <- paste0(envrmt$path_CDC_KL, "/", gsub("-", "", startDate), "_", gsub("-", "", endDate), "_", param, ".gpkg")
  if (!file.exists(outname)) {
    
  # Zielverzeichnis sicherstellen
  if (!dir.exists(envrmt$path_CDC_KL)) dir.create(envrmt$path_CDC_KL, recursive = TRUE)
  
  # Pfad zur gecachten Datei
  filename <- paste0(gsub("-", "", startDate), "_", gsub("-", "", endDate), "_", param, ".gpkg")
  result_path <- file.path(envrmt$path_CDC_KL, filename)
  if (file.exists(result_path)) {
    message("✔️ Vorhandene Datei gefunden: ", filename)
    return(st_read(result_path, quiet = TRUE))
  }
  
  # --- ab hier unveränderter Code ---
  
  # Basis-URLs
  base_url_http <- paste0("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/", reso, "/", var, "/", type, "/")
  base_url <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/", reso, "/", var, "/", type, "/")
  
  # Datei-Listing
  file_listing <- read_html(base_url_http) |>
    html_elements("a") |>
    html_attr("href")
  
  zip_files <- grep("^tageswerte_.*\\.zip$", file_listing, value = TRUE)
  if (length(zip_files) == 0) stop("⚠️ Keine ZIP-Dateien gefunden.")
  
  # Metadaten laden
  station_file <- grep("Beschreibung_Stationen", file_listing, value = TRUE)
  if (length(station_file) == 0) stop("⚠️ Keine Stationsbeschreibung gefunden.")
  
  station_url <- paste0(base_url, station_file)
  meta_df <- read.fwf(
    station_url,
    widths = c(6, 9, 9, 15, 12, 10, 41, 41, 4),
    skip = 2,
    col.names = c("Stations_id", "von_datum", "bis_datum", "Stationshoehe",
                  "geoBreite", "geoLaenge", "Stationsname", "Bundesland", "Abgabe"),
    strip.white = TRUE,
    fileEncoding = "Latin1"
  )
  
  # ZIP-Dateien herunterladen und entpacken
  for (zf in zip_files) {
    zip_url <- paste0(base_url, zf)
    zip_path <- file.path(envrmt$path_CDC_KL, basename(zf))
    
    if (!file.exists(zip_path)) {
      download.file(zip_url, destfile = zip_path, mode = "wb", quiet = TRUE)
    } else {
      message(paste("✔️ Vorhanden:", basename(zf)))
    }
    
    exdir <- file.path(envrmt$path_CDC_KL, tools::file_path_sans_ext(basename(zf)))
    if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
    unzip(zip_path, exdir = exdir)
  }
  
  # 🔁 produkt_klima_tag-Dateien aus allen Unterordnern
  unzipped_dirs <- list.dirs(envrmt$path_CDC_KL, recursive = TRUE, full.names = TRUE)
  produkt_files <- list.files(
    path = unzipped_dirs,
    pattern = "^produkt_klima_tag_.*\\.txt$",
    full.names = TRUE,
    recursive = FALSE
  )
  
  message("📂 Lese und filtere alle produkt_klima_tag_*.txt-Dateien – das kann einige Minuten dauern ...")
  
  
  data_list <- list()
  for (txt_file_full in produkt_files) {
    df <- tryCatch(
      read.table(txt_file_full, sep = ";", header = TRUE, na.strings = c("-999", "-999.0")),
      error = function(e) return(NULL)
    )
    
    if (!is.null(df) && param %in% names(df)) {
      df$MESS_DATUM <- as.Date(as.character(df$MESS_DATUM), format = "%Y%m%d")
      df <- df[df$MESS_DATUM >= as.Date(startDate) & df$MESS_DATUM <= as.Date(endDate), ]
      df <- df[, c("STATIONS_ID", "MESS_DATUM", param)]
      data_list[[length(data_list) + 1]] <- df
    }
  }
  
  var_df <- rbindlist(data_list)
  if (nrow(var_df) == 0) stop("⚠️ Kein Parameterwert in gewünschtem Zeitraum gefunden.")
  
  # Spatial Join
  # Spatial Join
  meta_df$geoBreite <- as.numeric(meta_df$geoBreite)
  meta_df$geoLaenge <- as.numeric(meta_df$geoLaenge)
  
  # Vereinheitliche Namen und Typen
  names(meta_df)[names(meta_df) == "Stations_id"] <- "STATIONS_ID"
  meta_df$STATIONS_ID <- as.numeric(meta_df$STATIONS_ID)
  var_df$STATIONS_ID <- as.numeric(var_df$STATIONS_ID)
  
  # Spatial DataFrame
  stations_sf <- st_as_sf(meta_df, coords = c("geoLaenge", "geoBreite"), crs = 4326)
  
  # Merge Klimawerte mit Metadaten
  merge_sf <- merge(stations_sf,var_df, by = "STATIONS_ID")
  
  message("::: Entferne unvollständige Einträge :::")
  n_before <- nrow(merge_sf)

  merge_sf <- merge_sf[complete.cases(st_drop_geometry(merge_sf)), ]
  n_after <- nrow(merge_sf)
  message(paste("⚠️ Gefiltert:", n_before - n_after, "Einträge mit NA"))
  message(paste("✅ Verbleibende Einträge:", n_after))
  message(paste("📡 Anzahl eindeutiger Stationen:", length(unique(merge_sf$STATIONS_ID))))
  
  # Speichern, wenn noch nicht vorhanden
  outname <- paste0(envrmt$path_CDC_KL, "/", gsub("-", "", startDate), "_", gsub("-", "", endDate), "_", param, ".gpkg")
  if (!file.exists(outname)) {
    message(paste("💾 Datei wird gespeichert unter:", outname))
    st_write(merge_sf, outname, delete_dsn = TRUE, quiet = TRUE)
  } else {
    message(paste("📁 Datei existiert bereits:", outname))
  }
  
  return(st_as_sf(merge_sf))
  }
  merge_sf = st_read(outname)
  message(paste("📁 Existierende Datei \n", outname, " \n wird verwendet"))
  return(merge_sf)
}

#' Klassifiziere Kriging-Unsicherheit in Qualitätsstufen und speichere Raster
#'
#' @param stars_obj stars-Objekt mit mindestens dem Layer "var1.var"
#' @param export_path Zeichenkette: vollständiger Dateipfad für das Output-Raster (optional)
#' @param compress Option für GeoTIFF-Kompression (default: "LZW")
#' @return stars-Objekt mit Qualitätsklassen als Faktor
#' @export
classify_kriging_uncertainty <- function(stars_obj, export_path = NULL, compress = "LZW") {
  require(stars)
  
  # Prüfen, ob Layer "var1.var" vorhanden ist
  if (!"var1.var" %in% names(stars_obj)) {
    stop("Layer 'var1.var' fehlt im stars-Objekt")
  }
  
  # Berechne die Standardabweichung σ = sqrt(var)
  sigma_raster <- sqrt(stars_obj[["var1.var"]])
  
  # Klassifiziere Unsicherheiten in vier Kategorien:
  # hoch:        σ < 1
  # mittel:      1 ≤ σ < 2
  # niedrig:     2 ≤ σ < 4
  # sehr niedrig: σ ≥ 4
  quality_class <- cut(
    sigma_raster,
    breaks = c(0, 1, 2, 4, Inf),
    labels = c("hoch", "mittel", "niedrig", "sehr niedrig"),
    right = FALSE
  )
  
  # Wandle in stars-Objekt um
  quality_raster <- st_as_stars(quality_class)
  names(quality_raster) <- "kriging_quality"
  
  # Optional speichern
  if (!is.null(export_path)) {
    stars::write_stars(quality_raster, export_path, overwrite = TRUE, options = paste0("COMPRESS=", compress))
  }
  
  return(quality_raster)
}
#' Bereinigt und begrenzt Klimadaten für einen gegebenen Parameter
#'
#' @param sf_day sf-Objekt mit Tageswerten eines Parameters
#' @param cVar Zeichenkette des DWD-KL-Parameters (z. B. "TXK", "RSK", ...)
#' @param date Datum im Format "YYYY-MM-DD" (nur erforderlich für SDK)
#' @return sf-Objekt mit neuer Spalte `tmp`, die die bereinigten Werte enthält
sanitize_climate_param <- function(sf_day, cVar, date = NULL) {
  x <- as.numeric(sf_day[[cVar]])
  
  # Fehldatenwert -999 zu NA
  x[x == -999] <- NA
  
  if (cVar == "SDK" && !is.null(date)) {
    # Sonnenauf- und untergang → maximale Tageslänge (in Stunden)
    dt <- suncalc::getSunlightTimes(date = as.Date(date), lat = 51.0, lon = 9.0, tz = "UTC")
    td <- dt$sunset - dt$sunrise
    maxDaylight <- ceiling(as.numeric(td))
    x[x > maxDaylight] <- maxDaylight
  }
  
  if (cVar == "PM") {
    x[x > 1060.6] <- 1060.6
    x[x < 954.9]  <- 954.9
  }
  
  if (cVar == "UPM") {
    x[x > 100] <- 100
    x[x < 0]   <- 0
  }
  
  if (cVar %in% c("TXK", "TNK", "TMK")) {
    x[x > 42]    <- 42
    x[x < -46.0] <- -46.0
  }
  
  if (cVar == "NM") {
    x[x > 8.0] <- 8.0
    x[x < 0]   <- 0
  }
  
  if (cVar == "RSK") {
    x[x > 312] <- 312
    x[x < 0]   <- 0
  }
  
  sf_day$tmp <- x
  return(sf_day)
}

#' @title Prepare German Elevation and Boundary Data
#'
#' @description
#' Downloads and processes DEM and administrative boundary data for Germany.
#' The result is a ready-to-use elevation raster (`dem`) for kriging or spatial modeling, saved as `dem.rds`.
#'
#' @param envrmt A named list of project paths, e.g. from `envimaR::createEnv()`.
#' @param bl Character: name of the federal state to extract (e.g. "Hessen").
#' @param crs Coordinate reference system (e.g. EPSG code or `terra::crs` object).
#' @param res Numeric: target grid resolution in meters (e.g. 500).
#' @param downloadDEM Logical: if `TRUE`, downloads and processes SRTM DEM; if `FALSE`, loads existing file.
#'
#' @return A `stars` object representing the processed DEM.
#' @export
#'
#' @author Chris Reudenbach
#' @examples
#' \dontrun{
#' envrmt <- createEnv()
#' dem <- prepare_DEM(envrmt, bl = "Hessen", crs = 3035, res = 500, downloadDEM = TRUE)
#' }

prepare_DEM <- function(envrmt, bl, crs, res = 500, downloadDEM = TRUE) {
  message("::: Load boundary data via geodata :::")

  
  # Load administrative boundaries
  germany <- geodata::gadm(country = "DEU", level = 1, path = tempdir())
  germany.sf <- st_as_sf(germany)
  germany.sf <- st_transform(germany.sf, crs = crs)
  
  # Filter selected state
  bl_sp <- germany[germany$NAME_1 == bl, ]
  bl_sf <- st_as_sf(bl_sp)
  bl_sf <- st_transform(bl_sf, crs = crs)
  
  # Dissolve all DE states into a unified polygon
  states_special <- c("Baden-Württemberg", "Nordrhein-Westfalen", "Hessen", "Bayern",
                      "Niedersachsen", "Sachsen-Anhalt", "Rheinland-Pfalz", "Sachsen",
                      "Mecklenburg-Vorpommern", "Schleswig-Holstein", "Brandenburg",
                      "Thüringen", "Saarland", "Berlin", "Hamburg", "Bremen")
  DE.states <- germany.sf[germany.sf$NAME_1 %in% states_special, ]
  DE <- DE.states %>% group_by(NAME_1) %>% summarize()
  
  if (downloadDEM) {
    message("::: Download and prepare SRTM elevation data :::")
    de_4326 <- st_transform(DE, 4326)
    st_write(de_4326, file.path(envrmt$path_data_lev0, "de_4326.shp"), delete_dsn = TRUE)
    
    download.url <- "https://opendem.info/downloads/srtm_germany_dtm.zip"
    zipfile <- file.path(envrmt$path_data_lev0, "srtm_germany_dtm.zip")
    download.file(download.url, zipfile, mode = "wb")
    unzip(zipfile, exdir = envrmt$path_data_lev0)
    
    srtm.germany <- terra::mask(
      terra::rast(file.path(envrmt$path_data_lev0, "srtm_germany_dtm.tif")),
      de_4326
    )
    
    message("::: Create template raster :::")
    grid.DE <- expand.grid(
      x = seq(round(st_bbox(germany.sf)["xmin"]), round(st_bbox(germany.sf)["xmax"]), by = res),
      y = seq(round(st_bbox(germany.sf)["ymin"]), round(st_bbox(germany.sf)["ymax"]), by = res)
    )
    coordinates(grid.DE) <- ~x + y
    crs(grid.DE) <- crs
    template_raster <- rasterFromXYZ(grid.DE, crs = crs)
    
    srtm.germany <- terra::project(srtm.germany, crs(template_raster))
    srtm500 <- terra::resample(srtm.germany, rast(template_raster))
    srtm500 <- round(srtm500, 0)
    names(srtm500) <- "Stationshoehe"
    
    dem <- st_as_stars(srtm500)
    saveRDS(dem, file.path(envrmt$path_data_lev0, "dem.rds"))
    rm(srtm.germany, srtm500, template_raster, grid.DE)
  }
  
  dem <- readRDS(file.path(envrmt$path_data_lev0, "dem.rds"))
  return(dem)
}

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


# ---- CSV export helper ----
write_stat_csv <- function(sf_df, file,dig) {
  df <- st_drop_geometry(sf_df)
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(df[numeric_cols], round, digits = dig)
  var_fin <- sjmisc::replace_columns(sf_df, df)
  data.table::fwrite(st_drop_geometry(var_fin), file = file, dec = ".")
}

correct_daytime = function(fn){
  for (f in fn){
    current = terra::rast(f)
    dt=suncalc::getSunlightTimes(date = as.Date(substr(basename(f),1,10)), lat = 51.0, lon = 9.0, tz = "UTC")
    td=dt$sunset-dt$sunrise
    maxDaylight= ceiling(as.numeric(unlist(stringr::str_split(td,"Time difference of "))))
    m <- c(-999, 0,0, maxDaylight,99999,maxDaylight)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    current <- terra::classify(current, rclmat, include.lowest=TRUE)
    names(current) = xfun::sans_ext(basename(f))
    if (calc_bl) 
    {  
      # Calculate data frame of min and max precipitation for all months
      var <- cbind(bl_sf, exactextractr::exact_extract(terra::rast(f), bl_sf, c("min", "max","count","majority","median","quantile","minority","variance","stdev","coefficient_of_variation"),quantiles = c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.25,0.5,0.75,0.9)))
      var$date = substr(tools::file_path_sans_ext(basename(f)),1,10)
      #c("min",	"max",	"count",	"majority",	"median",	"q10",	"q20",	"q30",	"q40",	"q60",	"q70",	"q80",	"q25",	"q50",	"q75",	"q90",	"minority","variance","stdev","coefficient_of_variation")
      vr=st_drop_geometry(var[,c("min",	"max",	"count",	"majority",	"median",	"q10",	"q20",	"q30",	"q40",	"q60",	"q70",	"q80",	"q25",	"q50",	"q75",	"q90",	"minority")]) %>%
        mutate_if(is.numeric, round, digits=dig)
      var_fin=sjmisc::replace_columns(var,vr)
      #saveRDS(var,file.path(envrmt$path_data_lev2,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".rds")))
      data.table::fwrite(st_drop_geometry(var_fin),file=file.path(envrmt$path_data_lev2,bl,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".csv")),dec = ".")
      current = terra::mask(raster::raster(f), bl_sf)
      current = terra::crop(current,bl_sf)
      raster::writeRaster(current,file.path(envrmt$path_data_lev1,bl,cVar,paste0(bl,basename(f))),overwrite=TRUE)
    }  else {
      writeRaster(current, f,gdal=c("COMPRESS=NONE"),overwrite=TRUE)
    }
  }
}

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
#' @param cVar [character] name of the climate variable (e.g., "TMK", "RSK")
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
#' @author
#' Chris Reudenbach, \email{creuden@@gmail.com}
extract_climate_statistics <- function(cVar,
                                       envrmt,
                                       calc_commu = TRUE,
                                       calc_bl = FALSE,
                                       bl = NULL,
                                       gemeinden_sf_3035,
                                       bl_sf = NULL,
                                       common_quantiles = c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.25, 0.5, 0.75, 0.9),
                                       dig = 1) {
  
  # Read climate files
  clim_files <- sort(list.files(file.path(envrmt$path_data_lev1, cVar), pattern = paste0("*", cVar, "\\.tif$"), full.names = TRUE))
  
  # Create output directories
  dir.create(file.path(envrmt$path_data_lev2, cVar), recursive = TRUE, showWarnings = FALSE)
  if (calc_bl) {
    dir.create(file.path(envrmt$path_data_lev1, bl, cVar), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(envrmt$path_data_lev2, bl, cVar), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Loop over climate raster files
  parallel::mclapply(seq_along(clim_files), function(i) {
   # for (i in seq_along(clim_files)) {   
    outfile_commu <- file.path(envrmt$path_data_lev2, cVar, paste0(tools::file_path_sans_ext(basename(clim_files[i])), ".csv"))
    #if (file.exists(outfile_commu)) return(NULL)
    
    if (cVar == "SDK") {
      correct_daytime(fn = clim_files[i])
     # return(NULL)
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
                                     quantiles = common_quantiles)
      )
      stat_vars$date <- substr(tools::file_path_sans_ext(basename(clim_files[i])), 1, 10)
      
      out_bl_file <- file.path(envrmt$path_data_lev2, bl, cVar, paste0(tools::file_path_sans_ext(basename(clim_files[i])), ".csv"))
      write_stat_csv(stat_vars, out_bl_file,dig)
      
      masked <- terra::mask(current, bl_sf)
      cropped <- terra::crop(masked, bl_sf)
      raster::writeRaster(cropped, file.path(envrmt$path_data_lev1, bl, cVar, paste0(bl, basename(clim_files[i]))), overwrite = TRUE)
    }
    
    if (calc_commu) {
      stat_vars <- cbind(
        gemeinden_sf_3035,
        exactextractr::exact_extract(current, gemeinden_sf_3035,
                                     c("min", "max", "count", "majority", "median", "quantile", "minority", "variance", "stdev", "coefficient_of_variation"),
                                     quantiles = common_quantiles)
      )
      stat_vars$date <- substr(tools::file_path_sans_ext(basename(clim_files[i])), 1, 10)
      write_stat_csv(stat_vars, outfile_commu,dig)
    }
    
  }, mc.cores = 16, mc.allow.recursive = TRUE)
  #} 
  # Merge CSV outputs
  if (calc_bl) {
    system(paste0("head -n 1 ", envrmt$path_data_lev2, "/", bl, cVar, "/2003-01-01_", cVar, ".csv > ",
                  envrmt$path_data_lev2, "/", bl, cVar, "/", bl, cVar, "_2003-2021.out && tail -n+2 -q ",
                  envrmt$path_data_lev2, "/", bl, cVar, "/*", cVar, ".csv >> ",
                  envrmt$path_data_lev2, "/", bl, cVar, "/", bl, cVar, "_2003-2021.out"), intern = FALSE)
  }
  if (calc_commu) {
    # system(paste0("head -n 1 ", envrmt$path_data_lev2, "/", cVar, "/2003-01-01_", cVar, ".csv > ",
    #               envrmt$path_data_lev2, "/", cVar, "/", cVar, "_2003-2021.out && tail -n+2 -q ",
    #               envrmt$path_data_lev2, "/", cVar, "/*", cVar, ".csv >> ",
    #               envrmt$path_data_lev2, "/", cVar, "/", cVar, "_2003-2021.out"), intern = FALSE)
    # Dynamisch Start- und Enddatum aus Dateinamen ableiten
    dates <- substr(tools::file_path_sans_ext(basename(clim_files)), 1, 10)
    start_date <- min(dates)
    end_date <- max(dates)
    
    # Ausgabedateinamen generieren
    merged_file_bl <- file.path(envrmt$path_data_lev2, bl, cVar, paste0(bl, cVar, "_", start_date, "-", end_date, ".out"))
    merged_file_commu <- file.path(envrmt$path_data_lev2, cVar, paste0(cVar, "_", start_date, "-", end_date, ".out"))
    
    # Zusammenführen für BL
    if (calc_bl) {
      first_file_bl <- file.path(envrmt$path_data_lev2, bl, cVar, paste0(start_date, "_", cVar, ".csv"))
      system(paste0("head -n 1 ", first_file_bl, " > ", merged_file_bl, 
                    " && tail -n+2 -q ", envrmt$path_data_lev2, "/", bl, cVar, "/*", cVar, ".csv >> ", merged_file_bl))
    }
    
    # Zusammenführen für Gemeinden
    if (calc_commu) {
      first_file_commu <- file.path(envrmt$path_data_lev2, cVar, paste0(start_date, "_", cVar, ".csv"))
      system(paste0("head -n 1 ", first_file_commu, " > ", merged_file_commu, 
                    " && tail -n+2 -q ", envrmt$path_data_lev2, "/", cVar, "/*", cVar, ".csv >> ", merged_file_commu))
    }
    
  }
}

  