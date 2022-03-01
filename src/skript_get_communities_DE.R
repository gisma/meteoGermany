#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]

#devtools::install_github("envima/envimaR")
library(envimaR)
library(rprojroot)
appendpackagesToLoad= c("downloader")
appendProjectDirList = c("data/data_lev0/GhcnDaily","data/data_lev0/GhcnMonthly")
root_folder = find_rstudio_root_file()

source(file.path(root_folder, "src/functions/000_setup.R"))

crs = raster::crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
epsg=3035
res=500
startYear = 2000

if (!file.exists(paste0(envrmt$path_data,"ref-nuts-2016-01m.geojson.zip")))
  download(url = "https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2016-01m.geojson.zip",
           destfile = paste0(envrmt$path_data,"ref-nuts-2016-01m.geojson.zip"))
unzip(zipfile = paste0(envrmt$path_data,"ref-nuts-2016-01m.geojson.zip"),
      exdir = envrmt$path_data,
      overwrite = TRUE)
# mit dem Paket sf und der Funktion sf_read lesen wir sie in eine Variable
nuts3 = st_read(paste0(envrmt$path_data,"NUTS_RG_01M_2016_3857_LEVL_3.geojson"))
# Deutschland-Kreise durch data frame Filterung auf Wert "DE" in Spalte CNTR_CODE
nuts3_de = nuts3[nuts3$CNTR_CODE=="DE",]
# Projektion der Geometriedaten von Pseudo-Merkator 3857 in ETRS89-extended / LAEA Europe 3035
nuts3_3035 = st_transform(nuts3_de, 3035)

# ---- Offizielle Zuweisungstabellen für Lokale Verwaltungseinheiten (LAU)  = > NUTS3 Konversion (eurostat)
# https://ec.europa.eu/eurostat/de/web/nuts/local-administrative-units
if (!file.exists(paste0(envrmt$path_data,"EU-28-LAU-2019-NUTS-2016.xlsx")))
  download(url = "https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx",
           destfile =paste0(envrmt$path_data,"EU-28-LAU-2019-NUTS-2016.xlsx"))
# Einlesen der xlsx Exceldatei (Daten für Deutschland sind im Datenblatt (=sheet) "DE")
conv_lau_nuts3 = readxl::read_xlsx(path.expand(paste0(envrmt$path_data,"EU-28-LAU-2019-NUTS-2016.xlsx")),
                                   sheet = "DE")

# ----  Offizielle Geometriedaten der Gemeindeflächen  (Bundesamt für Geodäsie und Kartographie)
# https://gdz.bkg.bund.de/index.php/default/open-data/verwaltungsgebiete-1-250-000-mit-einwohnerzahlen-ebenen-stand-31-12-vg250-ew-ebenen-31-12.html
if (!file.exists(paste0(envrmt$path_data,"gemeinden.zip")))
  download(url ="https://daten.gdz.bkg.bund.de/produkte/vg/vg250-ew_ebenen_1231/aktuell/vg250-ew_12-31.tm32.shape.ebenen.zip",
           destfile = paste0(envrmt$path_data,"gemeinden.zip"))
# Entpackt werden nur die benötigten Dateien (da es das SHP-Format handelt sind es mindestens 3 + Projektion also diese vier)
unzip(zipfile = paste0(envrmt$path_data,"gemeinden.zip"),
      files = c("vg250-ew_12-31.tm32.shape.ebenen/vg250-ew_ebenen_1231/VG250_GEM.shp",
                "vg250-ew_12-31.tm32.shape.ebenen/vg250-ew_ebenen_1231/VG250_GEM.dbf",
                "vg250-ew_12-31.tm32.shape.ebenen/vg250-ew_ebenen_1231/VG250_GEM.shx",
                "vg250-ew_12-31.tm32.shape.ebenen/vg250-ew_ebenen_1231/VG250_GEM.prj"),
      exdir = "gemeinden/",
      junkpaths = TRUE)
# Einlesen mit sf::sf_read
gemeinden_sf = st_read("gemeinden/VG250_GEM.shp")
# Projektion der Geometriedaten von ETRS89 / UTM zone 32N (N-E) 3044 in ETRS89-extended / LAEA Europe 3035
gemeinden_sf_3035 = st_transform(gemeinden_sf, 3035)
saveRDS(gemeinden_sf_3035,paste0(envrmt$path_data_lev1,"gemeinden_DE_3035.rds"))
# ---- Offizielle Gemeindeverzeichnisse (Statistische Bundeamt destatis)
# Die Gemeindeliste wird benötigt um die jeweils gültigen Gemeindenamen mit anderen Datenquellen zu verküpfen
# Dafür sind teils umfangreiche Säuberungsmaßnahmen notwendig
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/_inhalt.html
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/beschreibung-gebietseinheiten.pdf?__blob=publicationFile
# Laden der korrekten Gemeindeliste (Gemeinden > 5000 Einwohner ist kompatibel zu den Bertelsmann Daten)
# ACHTUNG Durch kontinuierliche Gebietreformen existieren zu unterschiedlichen Stichjahren unterschiedliche Gemeinden/Kreise etc.
# Einlesen erfolgt diesmal mit openxlsx::read.xlsx() zur einfacheren Steuerung der einzulesenden Matrix
gemeinde_liste_raw = openxlsx::read.xlsx("https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/Standardtabellen/07_GemeindenVorjahr.xlsx?__blob=publicationFile",
                                         sheet = "Gemeinden ab 5 000 Einwohnern",
                                         startRow = 8,
                                         colNames = FALSE,
                                         rowNames = FALSE,
                                         cols = c(2:7))
#- Einlesen und allgemeine Listen
# Erzeugen des LAU2 Codes aus den einzelnen Schlüsseln (siehe Datensatzbeschreibung)
gemeinde_liste_LAU=paste0(gemeinde_liste_raw$X1,gemeinde_liste_raw$X2,gemeinde_liste_raw$X3,gemeinde_liste_raw$X4,gemeinde_liste_raw$X5)

# Einlesen der Namensliste
gemeinde_liste_NAMES= stringr::str_split(gemeinde_liste_raw[1:nrow(gemeinde_liste_raw),6], ",",simplify = TRUE)[,1]

# LAU2 + "normale" Namensliste für späteren Gebrauch
gemeindeliste_combi=cbind(gemeinde_liste_LAU,gemeinde_liste_NAMES)

saveRDS(gemeindeliste_comb,paste0(envrmt$path_data_lev1,"LAU_Names.rds"))


altitude <- getData('alt', country = 'BRA')

prec_for_altitude <- exact_extract(prec[[12]], brazil, function(prec, frac, alt) {
  # ignore cells with unknown altitude
  prec <- prec[!is.na(alt)]
  frac <- frac[!is.na(alt)]
  alt <- alt[!is.na(alt)]

  low <- !is.na(alt) & alt < 500
  high <- !is.na(alt) & alt >= 500

  data.frame(
    prec_low_alt = weighted.mean(prec[low], frac[low]),
    prec_high_alt = weighted.mean(prec[high], frac[high])
  )
}, weights = altitude)
