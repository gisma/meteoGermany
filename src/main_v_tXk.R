#' Main control script
#'
#' @description controls the main prediction run. download and prepare climate and DEM data and perform a Kriging with autovariogram
#' https://rmets.onlinelibrary.wiley.com/doi/pdf/10.1017/S1350482706002362
#' @author Chris Reudenbach creuden@gmail.com
#devtools::install_github("envima/envimaR")
library(envimaR)
library(rprojroot)
appendpackagesToLoad= c("downloader","dplyr","readr","doParallel")
appendProjectDirList = c("data/data_lev0/GhcnDaily","data/data_lev0/GhcnMonthly")
root_folder = find_rstudio_root_file()

source(file.path(root_folder, "src/functions/000_setup.R"))

crs = raster::crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
epsg=3035
res=500
sample_size =50
startDate = "2003-01-01"
endDate = "2021-12-31"
type= "historical"#"historical"#"recent" #
# get data
source(file.path(envrmt$path_src,"prepare germany_data.R"))
dem = st_as_stars(srtm500)
cVar.sf = sf::st_as_sf(cVar.sp)
rm(srtm.germany,template_raster,grid.DE,srtm.germany.spdf,germany,DE.sp,srtm500,cVar.sp)
gc()

for (cVar in c("TXK","TNK","TMK")){ #,"SDK","PM","UPM")){
  cVar = "SDK"

  ##------------------ day data set
  dat_list = sort(as.character(unique(cVar.sf$MESS_DATUM)))[1:length(unique(cVar.sf$MESS_DATUM))]
  end=length(dat_list)
  matrix_of_sums <- parallel::mclapply( seq_along(dat_list), function(n){
    currentDate = dat_list[n]
    if (as.Date(currentDate) >= as.Date(startDate) & as.Date(currentDate) <= as.Date(endDate)){
      cd= substr(currentDate,1,10)
      if(!file.exists(paste0(envrmt$path_data_lev1,"/",cVar,"/",cd,"_",cVar,".tif"))){
        cVar.sf.day = cVar.sf[as.character(cVar.sf$MESS_DATUM) == as.Date(currentDate),]
        if (cVar == "SDK") {
          dt=suncalc::getSunlightTimes(date = as.Date(currentDate), lat = 51.0, lon = 9.0, tz = "UTC")
          td=dt$sunset-dt$sunrise
          maxDaylight= ceiling(as.numeric(unlist(stringr::str_split(td,"Time difference of "))))
          cVar.sf.day$tmp=NA
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) == -999, NA))
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) > maxDaylight, maxDaylight))
        } else if (cVar == "PM") {
          cVar.sf.day$tmp=NA
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) == -999, NA))
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) > 1060.6, 1060.6))
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) < 954.9, 954.9))
        } else if (cVar == "UPM") {
          cVar.sf.day$tmp=NA
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) == -999, NA))
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) > 100, 100))
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) < 0, 0))
        }else if (cVar == "TXK" | cVar == "TNK" | cVar == "TMK") {
          cVar.sf.day$tmp=NA
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) == -999, NA))
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) > 42, 42))
          dat = cVar.sf.day %>% mutate(tmp = replace(!!sym(cVar), as.numeric(!!sym(cVar)) < -46.0, -46.0))
        }

        dat = dat[,c("Stationshoehe","tmp","geometry")]
        dat$tmp=as.numeric(dat$tmp)
        names(dat) = c("Stationshoehe",cVar,"geometry")
        # cVar.sf.day %>% mutate(tmp = factor(ifelse(!!sym(cVar) == "SDK" & (!!sym(cVar) > 18 | !!sym(cVar) < 0), 1.233, 99, 88)))
        data <- dat %>% drop_na()
        if (nrow(data)>5){
          data <- dplyr::distinct(data, geometry, .keep_all = TRUE)
          data = st_transform(data,st_crs(dem))
          st_crs(dem)=3035
          st_crs(data)=3035
          seed=123

          vm.auto = automap::autofitVariogram(formula = as.formula(paste(cVar, "~1")),
                                              input_data = data)

          #plot(vm.auto)
          seed=123
          tmax.pred <- krige(formula = as.formula(paste(cVar, "~Stationshoehe")),
                             locations = data,#data[sample.int(nrow(data),min(sample_size,nrow(data))),],
                             newdata = dem,
                             model = vm.auto$var_model,
                             debug.level=-1)

          stars::write_stars(tmax.pred,paste0(envrmt$path_data_lev1,"/",cVar,"/",cd,"_",cVar,".tif"),overwrite=TRUE,options="COMPRESS=LZW")
          rm(tmax.pred)
          gc()
        } else {
          stars::write_stars(dem*0-9999,paste0(envrmt$path_data_lev1,"/",cVar,"/",cd,"_",cVar,".tif"),overwrite=TRUE,options="COMPRESS=LZW")
        }
      }

    }
  }, mc.cores = 10, mc.allow.recursive = TRUE)

  # final correction and extraction per community
  source(file.path(root_folder, "src/skript_calculate_communities_DE_climate.R"))
}
