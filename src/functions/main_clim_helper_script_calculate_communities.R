#' @description extraction and correction for irregular values of descriptive statistics of each community
#' @author Chris Reudenbach creuden@gmail.com


#gemeinden_sf_3035 = readRDS(paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.rds"))
#gemeindeliste_comb = readRDS(paste0(envrmt$path_data_lev0,"LAU_Names.rds"))
gemeinden_sf_3035 = st_read(paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.gpkg"))


# Create list of corresponding files
clim_files <- sort(list.files(paste0(envrmt$path_data_lev1,"/",cVar), paste0("*",cVar,"\\.tif$"), full.names = T),decreasing = F)
if (!dir.exists(paste0(envrmt$path_data_lev2,cVar)))
  dir.create(file.path(envrmt$path_data_lev2,cVar),recursive = TRUE)
if (calc_bl & !dir.exists(paste0(envrmt$path_data_lev1,bl,cVar)))
  dir.create(file.path(envrmt$path_data_lev1,bl,cVar),recursive = TRUE)
if (calc_bl & !dir.exists(paste0(envrmt$path_data_lev2,bl,cVar)))
  dir.create(file.path(envrmt$path_data_lev2,bl,cVar),recursive = TRUE)

matrix_of_sums <- parallel::mclapply( seq_along(clim_files), function(i){
  #for (i in seq_along(clim_files)){
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
      dig = 1
    }else if (cVar == "UPM") {
      current = terra::rast(clim_files[i])
      m <- c(-999, 0,0, 100,99999,100)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      current <- terra::classify(current, rclmat, include.lowest=TRUE)
      names(current) = xfun::sans_ext(basename(clim_files[i]))
#      writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
      dig = 0
    }else if (cVar == "TXK" | cVar == "TMK" |cVar == "TNK" ) {
      current = terra::rast(clim_files[i])
      m <- c(-999, -46,-46, 42,99999,42)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      current <- terra::classify(current, rclmat, include.lowest=TRUE)
      names(current) = xfun::sans_ext(basename(clim_files[i]))
 #     writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
      dig = 1
    }else if (cVar == "NM") {
      current = terra::rast(clim_files[i])
      m <- c( -1000,0,0, 8,99999,8)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      current <- terra::classify(current, rclmat, include.lowest=TRUE)
      names(current) = xfun::sans_ext(basename(clim_files[i]))
  #    writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
      dig = 1
    }else if (cVar == "RSK") {
      current = terra::rast(clim_files[i])
      m <- c(-1000,0 ,0, 312,99999,312)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      current <- terra::classify(current, rclmat, include.lowest=TRUE)
      names(current) = xfun::sans_ext(basename(clim_files[i]))
  #    writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
      dig = 1
    }
  if (calc_bl) 
  {  
    # Calculate data frame of min and max precipitation for all months
    stat_vars <- cbind(bl_sf, exactextractr::exact_extract(current, bl_sf, c("min", "max","count","majority","median","quantile","minority","variance","stdev","coefficient_of_variation"),quantiles = c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.25,0.5,0.75,0.9)))
    stat_vars$date = substr(tools::file_path_sans_ext(basename(clim_files[i])),1,10)
    vr=st_drop_geometry(stat_vars[,c("min",	"max",	"count",	"majority",	"median",	"q10",	"q20",	"q30",	"q40",	"q60",	"q70",	"q80",	"q25",	"q50",	"q75",	"q90",	"minority")]) %>%
      mutate_if(is.numeric, round, digits=dig)
    var_fin=sjmisc::replace_columns(stat_vars,vr)
    #saveRDS(stat_vars,file.path(envrmt$path_data_lev2,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".rds")))
    data.table::fwrite(st_drop_geometry(var_fin),file=file.path(envrmt$path_data_lev2,bl,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".csv")),dec = ".")
    current = terra::mask(current, bl_sf)
    current = terra::crop(current,bl_sf)
    raster::writeRaster(current,file.path(envrmt$path_data_lev1,bl,cVar,paste0(bl,basename(clim_files[i]))),overwrite=TRUE)
  }  else {
    writeRaster(current, clim_files[i],gdal=c("COMPRESS=NONE"),overwrite=TRUE)
  }
    if (calc_commu){
    # Calculate data frame of min and max precipitation for all months
    stat_vars <- cbind(gemeinden_sf_3035, exactextractr::exact_extract(raster::raster(clim_files[i]), gemeinden_sf_3035, c("min", "max","count","majority","median","quantile","minority","variance","stdev","coefficient_of_variation"),quantiles = c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.25,0.5,0.75,0.9)))
    stat_vars$date = substr(tools::file_path_sans_ext(basename(clim_files[i])),1,10)
    #c("min",	"max",	"count",	"majority",	"median",	"q10",	"q20",	"q30",	"q40",	"q60",	"q70",	"q80",	"q25",	"q50",	"q75",	"q90",	"minority","variance","stdev","coefficient_of_variation")
    vr=st_drop_geometry(stat_vars[,c("min",	"max",	"count",	"majority",	"median",	"q10",	"q20",	"q30",	"q40",	"q60",	"q70",	"q80",	"q25",	"q50",	"q75",	"q90",	"minority")]) %>%
      mutate_if(is.numeric, round, digits=dig)
    var_fin=sjmisc::replace_columns(stat_vars,vr)
    print(i)
    print(var_fin)
    #saveRDS(stat_vars,file.path(envrmt$path_data_lev2,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".rds")))
    data.table::fwrite(st_drop_geometry(var_fin),file=file.path(envrmt$path_data_lev2,cVar,paste0(tools::file_path_sans_ext(basename(clim_files[i])),".csv")),dec = ".")
    }
  
    }
}, mc.cores = 16, mc.allow.recursive = TRUE)

if (calc_bl)
  system(paste0("head -n 1 ",envrmt$path_data_lev2,"/",bl,cVar,"/2003-01-01_",cVar,".csv > ",envrmt$path_data_lev2,"/",bl,cVar,"/",cVar,"_2003-2021.out && tail -n+2 -q ",envrmt$path_data_lev2,"/",cVar,"/*",cVar,".csv >> ",envrmt$path_data_lev2,"/",bl,cVar,"/",bl,cVar,"_2003-2021.out"),intern =F)
#
if (calc_commu)
  system(paste0("head -n 1 ",envrmt$path_data_lev2,"/",cVar,"/2003-01-01_",cVar,".csv > ",envrmt$path_data_lev2,"/",cVar,"/",cVar,"_2003-2021.out && tail -n+2 -q ",envrmt$path_data_lev2,"/",cVar,"/*",cVar,".csv >> ",envrmt$path_data_lev2,"/",cVar,"/",cVar,"_2003-2021.out"),intern =F)
#system(paste0("7z a -tzip -v2G . ",envrmt$path_data_lev2,"/",cVar,"/",cVar,"_2003-2021.out"))
