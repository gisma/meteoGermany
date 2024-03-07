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


# ---- Mikrozensus Daten
# Download URLs der Mikrozensusdaten https://www.zensus2011.de/
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

fn  =  list.files(pattern = "100.[.]csv$", path = envrmt$path_data_lev0, full.names = TRUE)
#
# Die Dateien sind z.T. > 5 GB daher ist ein extrem schnelles Einlesen der Daten mit data.table zwingend
# erste Variable Grid-Kodierung + Gesamtbevölkerung
# erste Datei aus der fn-Liste  Demografie
# grid_bevoelkerung_2011 = as_tibble(data.table::fread(paste0(envrmt$path_data_lev0,"/Zensus_Bevoelkerung_100m.csv")))
# mz_demografie_2011 = as_tibble(data.table::fread(fn[4]))
# mz_familie_2011 = as_tibble(data.table::fread(fn[5]))
# mz_haushalte_2011 = as_tibble(data.table::fread(fn[6]))
# mz_wohnungen_2011 = as_tibble(data.table::fread(fn[2]))
# mz_haus_2011 = as_tibble(data.table::fread(fn[1]))

# ACHTUNG die sqlite Datei ist > 18 GB!
mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(envrmt$path_data_lev0,"/mikrozensus2011_BD_2.sqlite"),cache_size = "2000000")
dbWriteTable(mydb, "grid_bevoelkerung_2011", data.table::fread(fn[3]),overwrite= TRUE)
dbWriteTable(mydb, "mz_demografie_2011", data.table::fread(fn[4]),overwrite= TRUE)
dbWriteTable(mydb, "mz_familie_2011", data.table::fread(fn[5]),overwrite= TRUE)
dbWriteTable(mydb, "mz_haushalte_2011", data.table::fread(fn[6]),overwrite= TRUE)
dbWriteTable(mydb, "mz_wohnungen_2011", data.table::fread(fn[2]),overwrite= TRUE)
dbWriteTable(mydb, "mz_haus_2011", data.table::fread(fn[1]),overwrite= TRUE)

# listen aller enthaltenen tables
dbListTables(mydb)
DBI::dbDisconnect()
