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
