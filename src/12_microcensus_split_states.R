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
mz_id = "fami"

gemeinden_sf_3035 = st_read(paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.gpkg"))
# grid_demo_DE_sf = readRDS(paste0(envrmt$path_data_lev1,"/grid_demo_DE_sf.rds"))
# split_mz_data (poly= gemeinden_sf_3035, mz_data=grid_demo_DE_sf,state_id = state_id,out_id="mz")

# grid_mz_DE_sf = readRDS(paste0(envrmt$path_data_lev1,"/grid_","fami","_DE_sf.rds"))
# split_mz_data (poly= gemeinden_sf_3035, mz_data=grid_mz_DE_sf,state_id = state_id,out_id="fami")

# grid_mz_DE_sf = readRDS(paste0(envrmt$path_data_lev1,"/grid_","wohn","_DE_sf.rds"))
# split_mz_data (poly= gemeinden_sf_3035, mz_data=grid_mz_DE_sf,state_id = state_id,out_id="wohn")

 grid_mz_DE_sf = readRDS(paste0(envrmt$path_data_lev1,"/grid_","haus","_DE_sf.rds"))
 split_mz_data (poly= gemeinden_sf_3035, mz_data=grid_mz_DE_sf,state_id = state_id,out_id="haus")

# NOT CLEAR WHAT IS THE DIFFERENCE TO WOHN
## grid_mz_DE_sf = readRDS(paste0(envrmt$path_data_lev1,"/grid_","geb","_DE_sf.rds"))
## split_mz_data (poly= gemeinden_sf_3035, mz_data=grid_mz_DE_sf,state_id = state_id,out_id="geb")
