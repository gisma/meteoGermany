#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]

#devtools::install_github("envima/envimaR")
library(envimaR)
library(rprojroot)
library(DBI)
appendpackagesToLoad= c("downloader")
appendProjectDirList = c("data/data_lev0/GhcnDaily","data/data_lev0/GhcnMonthly")
root_folder = find_rstudio_root_file()

source(file.path(root_folder, "src/functions/000_setup.R"))

crs = raster::crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
epsg=3035


gemeinden_sf_3035 = readRDS(paste0(envrmt$path_data_lev0,"gemeinden_DE_3035.rds"))
#gemeindeliste_comb = readRDS(paste0(envrmt$path_data_lev1,"LAU_Names.rds"))

# ---- Mikrozensus Daten
# Download URLs der Mikrozensusdaten https://www.zensus2011.de/
# Exemplarisch wird nur mit den Kategorien Bevoelkerung und Demographie gearbeitet
# Die csv_Bevoelkerung_100m_Gitter.zip Datei enthält die Geokoordinaten
# als x y Spalten in der Refrenzierung ETRS89-extended / LAEA Europe 3035
# # Bei Bedarf können die auskommentierten URLs aktiviert werden
# url=list()
# url$gitter = "https://daten.gdz.bkg.bund.de/produkte/sonstige/geogitter/aktuell/DE_Grid_ETRS89-LAEA_100m.gpkg.zip"
# url$demo_grund_2011="https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip?__blob=publicationFile&v=3"
# url$demographie_2011="https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Demographie_100m_Gitter.zip?__blob=publicationFile&v=2"
# url$familien_2011="https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Familien_100m_Gitter.zip?__blob=publicationFile&v=2"
# url$haushalte_2011="https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Haushalte_100m_Gitter.zip?__blob=publicationFile&v=2"
# url$haueser_2011="https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Gebaeude_100m_Gitter.zip?__blob=publicationFile&v=2"
# url$wohnungen_2011="https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Wohnungen_100m_Gitter.zip?__blob=publicationFile&v=5"
#
# # Download und Entpacken der Mikrozensus 2011 Daten. Geht natürlich auch manuell
# for (mzu in url){
#   # Download hierzu wird die URL aus der Liste einzeln in der Variable mzu verwendet der Ausgabedateiname wird durch paste0(...) erzeugt
#   if (!file.exists( paste0(envrmt$path_data_lev0,"/",strsplit(basename(mzu),".",fixed=TRUE)[[1]][1],".zip")))
#     res  =  curl::curl_download(mzu, paste0(envrmt$path_data_lev0,"/",strsplit(basename(mzu),".",fixed=TRUE)[[1]][1],".zip"), quiet = FALSE)
#   # Das Entpacken muss evtl. manuell durchgeführt werden hier wird mit 7zip gearbeitet, das auf dem OS installiert sein muss
#   print("Das Entpacken muss evtl. von Hand durchgeführt werden, \nda je nach Betriebssystem > 4GB Darteien von R nicht korrekt entpackt werden können\nund daher 7zip installiert sein muss")
#   # Entzippen hier mit einem sogenannten Kommandozeilen-Aufruf über die Funktion system()
#   # Der zusammengesetzte Textstring paste(...) ist ein Befehlsaufruf der in der Shell die externe Software 7zip startet
#   # system(paste0("7z e -o.", " ", paste0(envrmt$path_data_lev0,"/",strsplit(basename(mzu),".",fixed=TRUE)[[1]][1],".zip")),
#   #        intern = FALSE,
#   #        ignore.stdout = FALSE,
#   #        ignore.stderr = TRUE,
#   #        wait = FALSE)
# }
# Erstellen einer Dateiliste (inkl. Pfad) die AUSSCHLIESSLICH die Mikrozensus csv Dateien enthält.
# Zum Filtern werden sog. regex Ausdrücke verwendet
# Liste im aktuellen Arbeitsverzeichnis und darunter liegenden Verzeichnissen ALLE Daten die  auf "100?.csv" enden
# Das "?" ist notwendig da die Dateien mal 100m und mal 100M enthalten
fn  =  list.files(pattern = "100.[.]csv$", path = envrmt$path_data_lev0, full.names = TRUE)

  # Die Dateien sind z.T. > 5 GB daher ist ein extrem schnelles Einlesen der Daten mit data.table zwingend
  # erste Variable Grid-Kodierung + Gesamtbevölkerung
  # erste Datei aus der fn-Liste  Demografie
  grid_bevoelkerung_2011 = as_tibble(data.table::fread(paste0(envrmt$path_data_lev0,"/Zensus_Bevoelkerung_100m.csv")))
  mz_demografie_2011 = as_tibble(data.table::fread(fn[2]))
  mz_familie_2011 = as_tibble(data.table::fread(fn[3]))
  mz_haushalte_2011 = as_tibble(data.table::fread(fn[4]))

  # ACHTUNG die aktuelle Datei ist > 10 GB!
  mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(envrmt$path_data_lev0,"/mikrozensus2011_BD_2.sqlite"),cache_size = "2000000")
  # dbWriteTable(mydb, "grid_bevoelkerung_2011", data.table::fread(fn[1]))
  # dbWriteTable(mydb, "mz_demografie_2011", data.table::fread(fn[2]))
  # dbWriteTable(mydb, "mz_familie_2011", data.table::fread(fn[3]))
  # dbWriteTable(mydb, "mz_haushalte_2011", data.table::fread(fn[4]))


  # listen aller enthaltenen tables
  dbListTables(mydb)


  # Zur Nutzung von dplyr das wesentlich einfacher zu bedienen ist als SQL
  # ist es sinnvoll via dplyr::tbl auf eine spezifische Tabelle zu verlinken
  grid_link  =  tbl(mydb,"grid_bevoelkerung_2011")
  demo_link  =  tbl(mydb, "mz_demografie_2011")
  fami_link  =  tbl(mydb, "mz_familie_2011")
  haus_link  =  tbl(mydb, "mz_haushalte_2011")
  # Zusammenführen der Informationen in eine Tabelle über die Schlüsselspalte "Gitter_ID_100m"
  # dplyr::inner_join()ordnet jeder y zeile eine X Zeile zu. ACHTUNG die Reihenfolge ist wichtig!
  # Geht natürlich auch mit tibble Tabellen Zum Einlesen der Daten als tibble() den Schalter mz_read=TRUE setzen
  # Beide folgenden  Aufrufe sind identisch. Das  %>%  collect() ist notwendig um die Daten physisch in die Variable zu schreiben

  gitter=st_read(paste0(envrmt$path_data_lev0,"/DE_Gitter_ETRS89_LAEA_1km.shp"))
  mz_2011_DE = grid_link %>%
    inner_join(demo_link,by = "Gitter_ID_100m") %>%  collect()
  # mz_2011_BD_2 = inner_join(demo_link,gb_link, by = "Gitter_ID_100m") %>%  collect()
  dbWriteTable(mydb, "mz_2011_DE", mz_2011_DE)
  mz_link  =  tbl(mydb, "mz_2011_DE")

  # Konvertieren der Tabelle in einen räumlichen sf Vektordatensatz Projektion 3035
  mz_2011_DE_sf = sf::st_as_sf( mz_2011_DE ,
                                       coords = c("x_mp_100m", "y_mp_100m"),
                                       crs = 3035,
                                       agr = "constant")
  grid.DE <- expand.grid(x = seq(from = round(st_bbox(gemeinden_sf_3035)["xmin"]),
                                 to = round(st_bbox(gemeinden_sf_3035)["xmax"]),
                                 by = 100),
                         y = seq(from = round(st_bbox(gemeinden_sf_3035)["ymin"]),
                                 to = round(st_bbox(gemeinden_sf_3035)["ymax"]),
                                 by = 100))
  coordinates(grid.DE) <- ~x + y
  crs(grid.DE)=crs

  # raster
  template_raster <-grid.DE %>%
    raster::rasterFromXYZ(
      crs = crs)
  values(template_raster)= 1
  plot(template_raster)
  writeRaster(template_raster,paste0(envrmt$path_data_lev0,"/raw.tif"),overwrite=T)
  st_write(gemeinden_sf_3035$SDV_ARS,paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.gpkg"))
  mask <- gdalUtils::gdal_rasterize(src_datasource=paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.gpkg"),
                                    dst_filename=paste0(envrmt$path_data_lev0,"/raw_.tif"),
                                    a="ARS",tr= c(100,100),
                                    output_Raster=TRUE)
  writeRaster(mask,paste0(envrmt$path_data_lev0,"/raw.tif"),overwrite=T)
  gemeinden_2011_DE_sf = st_intersection(gemeinden_sf_3035,mz_2011_DE_sf)

  sf::dbWriteTable(mydb, value=gemeinden_2011_DE_sf, name = "gemeinden_2011_DE_sf")
  MRBiKo =  read_sf(mydb, "gemeinden_2011_DE_sf")
  dbDisconnect(mydb)
