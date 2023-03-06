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

# via dplyr::tbl  link auf die spezifische Tabelle
mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(envrmt$path_data_lev0,"/mikrozensus2011_BD_2.sqlite"),cache_size = "2000000")
dbListTables(mydb)
grid_link  =  tbl(mydb,"grid_bevoelkerung_2011")
demo_link  =  tbl(mydb, "mz_demografie_2011")
fami_link  =  tbl(mydb, "mz_familie_2011")
haus_link  =  tbl(mydb, "mz_haushalte_2011")
geb_link  =  tbl(mydb, "mz_haus_2011")
wohn_link  =  tbl(mydb, "mz_wohnungen_2011")

# Zusammenführen der Informationen in eine Tabelle über die Schlüsselspalte "Gitter_ID_100m"
# dplyr::inner_join()ordnet jeder y zeile eine X Zeile zu. ACHTUNG die Reihenfolge ist wichtig!
# Geht natürlich auch mit tibble Tabellen Zum Einlesen der Daten als tibble() den Schalter mz_read=TRUE setzen
# Das  %>%  collect() ist notwendig um die Daten physisch in die Variable zu schreiben
# Konvertieren der Tabelle in einen räumlichen sf Vektordatensatz Projektion 3035

# soziodemographie
grid_demo_DE = grid_link %>%
  inner_join(demo_link,by = "Gitter_ID_100m") %>%  collect()
grid_demo_DE_sf = sf::st_as_sf( grid_demo_DE,
                                coords = c("x_mp_100m", "y_mp_100m"),
                                crs = 3035,
                                agr = "constant")
saveRDS(grid_demo_DE_sf,paste0(envrmt$path_data_lev1,"/grid_demo_DE_sf.rds"))
rm(grid_demo_DE_sf)
rm(grid_demo_DE)
# familien
grid_fami_DE = grid_link %>%
  inner_join(fami_link,by = "Gitter_ID_100m") %>%  collect()
grid_fami_DE_sf = sf::st_as_sf( grid_fami_DE,
                                coords = c("x_mp_100m", "y_mp_100m"),
                                crs = 3035,
                                agr = "constant")
saveRDS(grid_fami_DE_sf,paste0(envrmt$path_data_lev1,"/grid_fami_DE_sf.rds"))
rm(grid_fami_DE_sf)
rm(grid_fami_DE)

# haushalte
grid_haus_DE = grid_link %>%
  inner_join(haus_link,by = "Gitter_ID_100m") %>%  collect()
grid_haus_DE_sf = sf::st_as_sf( grid_haus_DE,
                                coords = c("x_mp_100m", "y_mp_100m"),
                                crs = 3035,
                                agr = "constant")
saveRDS(grid_haus_DE_sf,paste0(envrmt$path_data_lev1,"/grid_haus_DE_sf.rds"))
rm(grid_haus_DE_sf)
rm(grid_haus_DE)

# wohnungen
grid_wohn_DE = grid_link %>%
  inner_join(wohn_link,by = "Gitter_ID_100m") %>%  collect()
grid_wohn_DE_sf = sf::st_as_sf( grid_wohn_DE,
                                coords = c("x_mp_100m", "y_mp_100m"),
                                crs = 3035,
                                agr = "constant")
saveRDS(grid_wohn_DE_sf,paste0(envrmt$path_data_lev1,"/grid_wohn_DE_sf.rds"))
rm(grid_wohn_DE_sf)
rm(grid_haus_DE)

# gebäude
grid_geb_DE = grid_link %>%
  inner_join(geb_link,by = "Gitter_ID_100m") %>%  collect()
grid_geb_DE_sf = sf::st_as_sf( grid_geb_DE,
                               coords = c("x_mp_100m", "y_mp_100m"),
                               crs = 3035,
                               agr = "constant")
saveRDS(grid_geb_DE_sf,paste0(envrmt$path_data_lev1,"/grid_geb_DE_sf.rds"))
rm(grid_geb_DE_sf)
rm(grid_geb_DE)

