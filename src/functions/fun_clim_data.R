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




######

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
  library(rvest)
  library(sf)
  library(data.table)
  library(dplyr)
  
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
  
  message(paste("📁 Existierende Datei \n", outname, " \n wird verwendet"))
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

  