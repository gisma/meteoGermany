#' supporting script
#'
#' @description extraction and correction for irregular values of descriptive statistics of each community
#'
#' @author Chris Reudenbach creuden@gmail.com

#devtools::install_github("envima/envimaR")
# library(envimaR)
# library(rprojroot)
# appendpackagesToLoad= c("downloader","dplyr","readr")
# appendProjectDirList = c("data/data_lev0/GhcnDaily","data/data_lev0/GhcnMonthly")
# root_folder = find_rstudio_root_file()
# source(file.path(root_folder, "src/functions/000_setup.R"))

#gemeinden_sf_3035 = readRDS(paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.rds"))
#gemeindeliste_comb = readRDS(paste0(envrmt$path_data_lev0,"LAU_Names.rds"))
gemeinden_sf_3035 = st_read(paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.gpkg"))

#cVar ="TXK"
# Create list of corresponding files
clim_files <- sort(list.files(paste0(envrmt$path_data_lev1,"/",cVar), paste0("*",cVar,"\\.tif$"), full.names = T),decreasing = F)
if (!dir.exists(paste0(envrmt$path_data_lev2,cVar)))
  dir.create(file.path(envrmt$path_data_lev2,cVar),recursive = TRUE)

matrix_of_sums <- parallel::mclapply( seq_along(clim_files), function(i){
  if (!file.exists(file.path(envrmt$path_data_lev2,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".csv")))){
    if (cVar == "SDK") {
      correct_daytime(fn=clim_files[i])
      dig = 3
    } else if (cVar == "PM") {
      current = terra::rast(clim_files[i])
      m <- c(0, 954.9,954.9, 1060.6,99999,1060.6)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      current <- terra::classify(current, rclmat, include.lowest=TRUE)
      names(current) = xfun::sans_ext(basename(clim_files[i]))
      writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
      dig = 1
    }else if (cVar == "UPM") {
      current = terra::rast(clim_files[i])
      m <- c(-999, 0,0, 100,99999,100)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      current <- terra::classify(current, rclmat, include.lowest=TRUE)
      names(current) = xfun::sans_ext(basename(clim_files[i]))
      writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
      dig = 0
    }else if (cVar == "TXK" | cVar == "TMK" |cVar == "TNK" ) {
      current = terra::rast(clim_files[i])
      m <- c(-999, -46,-46, 42,99999,42)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      current <- terra::classify(current, rclmat, include.lowest=TRUE)
      names(current) = xfun::sans_ext(basename(clim_files[i]))
      writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
      dig = 1
    }

    # Calculate data frame of min and max precipitation for all months
    var <- cbind(gemeinden_sf_3035, exactextractr::exact_extract(raster::raster(clim_files[i]), gemeinden_sf_3035, c("min", "max","count","majority","median","quantile","minority","variance","stdev","coefficient_of_variation"),quantiles = c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.25,0.5,0.75,0.9)))
    var$date = substr(tools::file_path_sans_ext(basename(clim_files[i])),1,10)
    #c("min",	"max",	"count",	"majority",	"median",	"q10",	"q20",	"q30",	"q40",	"q60",	"q70",	"q80",	"q25",	"q50",	"q75",	"q90",	"minority","variance","stdev","coefficient_of_variation")
    vr=st_drop_geometry(var[,c("min",	"max",	"count",	"majority",	"median",	"q10",	"q20",	"q30",	"q40",	"q60",	"q70",	"q80",	"q25",	"q50",	"q75",	"q90",	"minority")]) %>%
      mutate_if(is.numeric, round, digits=dig)
    var_fin=sjmisc::replace_columns(var,vr)
    #saveRDS(var,file.path(envrmt$path_data_lev2,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".rds")))
    data.table::fwrite(st_drop_geometry(var_fin),file=file.path(envrmt$path_data_lev2,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".csv")),dec = ".")
  }
}, mc.cores = 12, mc.allow.recursive = TRUE)




# depreceated to slow to  memory consuming
# for (cVar in var_code){
#
# df <- sort(list.files(paste0(envrmt$path_data_lev2,"/",cVar), paste0("*",cVar,"\\.csv$"), full.names = T),decreasing = F) %>%
#   lapply(read_csv2) %>%
#   bind_rows
# write_csv2(df,file.path(envrmt$path_data_lev2,paste0(cVar,"_2000-2022.csv")))
# }

#var_code = c("TNK","TMK","SDK")
#for (cVar in var_code){
system(paste0("head -n 1 ",envrmt$path_data_lev2,"/",cVar,"/2003-01-01_",cVar,".csv > ",envrmt$path_data_lev2,"/",cVar,"/",cVar,"_2003-2021.out && tail -n+2 -q ",envrmt$path_data_lev2,"/",cVar,"/*",cVar,".csv >> ",envrmt$path_data_lev2,"/",cVar,"/",cVar,"_2003-2021.out"),intern =F)
system(paste0("7z a -tzip -v2G . ",envrmt$path_data_lev2,"/",cVar,"/",cVar,"_2003-2021.out"))
#")
#}
