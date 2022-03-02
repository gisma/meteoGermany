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
var_code = c("UPM")

gemeinden_sf_3035 = readRDS(paste0(envrmt$path_data_lev1,"gemeinden_DE_3035.rds"))
#gemeindeliste_comb = readRDS(paste0(envrmt$path_data_lev1,"LAU_Names.rds"))

vc=var_code

# calculate var stats for each community
for (vc in var_code){

  # Create list of corresponding files
  clim_files <- list.files(envrmt$path_data_lev1, paste0("*",vc,"\\.tif$"), full.names = T)
  if (!dir.exists(paste0(envrmt$path_data_lev2,vc)))
    dir.create(file.path(envrmt$path_data_lev2,vc),recursive = TRUE)
  for (i in 1:length(clim_files)){
    # Calculate data frame of min and max precipitation for all months
    var <- cbind(gemeinden_sf_3035, exactextractr::exact_extract(raster(clim_files[i]), gemeinden_sf_3035, c("min", "max","count","majority","median","quantile","minority","variance","stdev","coefficient_of_variation"),quantiles = c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.25,0.5,0.75,0.9)))
    var$date = substr(tools::file_path_sans_ext(basename(clim_files[i])),1,10)
    #saveRDS(var,file.path(envrmt$path_data_lev2,vc,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".rds")))
    write_csv2(st_drop_geometry(var),file.path(envrmt$path_data_lev2,vc,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".csv")))
  }

}
