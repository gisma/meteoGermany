split_mz_data = function(poly= NULL,mz_data=NULL,state_id = NULL){
for (s_id in state_id){
  # crop to state
  state = st_crop( gemeinden_sf_3035 %>% filter(substr(AGS,1,2)==s_id),gemeinden_sf_3035)
  mz_state = st_crop(grid_demo_DE_sf,state)
  saveRDS(mz_state,paste0(envrmt$path_data_lev0,"/",s_id,"_mz_state.rds"))
  saveRDS(state,paste0(envrmt$path_data_lev0,"/",s_id,"_state.rds"))
}}

cv.IDW  <- function(spatialDF, stat.formula = NULL,
                    seqNeighbors = NULL, seqBeta = NULL,
                    evalGridSize = NULL,
                    evalRaster = NULL,
                    verbose = TRUE){

  ### LOAD LIBRARIES ###
  library(sp)
  library(sf)
  library(gstat)
  library(raster)


  ### PROVIDE DEFAULT VALUES FOR FUNCTION ARGUMENTS ###
  if (is.null(seqNeighbors)){
    seqNeighbors <- round(seq(3, length(spatialDF), length.out = 10))
  }
  if (is.null(seqBeta)){
    seqBeta <- c(0.1, seq(0.5, 3, 0.5))
  }
  # if (is.null(evalGridSize)){
  #   x.interval <- extent.DE["xmax"] - extent.DE["xmin"]
  #   y.interval <- extent.DE["ymax"] - extent.DE["ymin"]
  #   evalGridSize <- round(min(x.interval, y.interval) *0.05)
  # }
  if (is.null(stat.formula)){
    print('Please provide a formula!!')
    return()
  }
  # if (is.null(evalRaster)){
  #   extent.evalGrid <- raster::extent(spatialDF)
  # }else{
  #   extent.evalGrid <- raster::extent(evalRaster)
  # }


  ### BUILD A GRID FOR PARAMETER COMBINATIONS ###
  cv.Grid <- expand.grid(Beta = seqBeta,
                         Neighbors = seqNeighbors)
  cv.Grid$RMSE <- NA
  spatialDF = spTransform(spatialDF,crs)
  ### LOOP THROUGH ALL PARAMETER COMBINATIONS ###
  for (i in 1:nrow(cv.Grid)){
    ### BUILD IDW MODEL ###
    idw <- gstat(formula = stat.formula,
                 data = spatialDF,
                 nmax = cv.Grid[i, 'Neighbors'],
                 set = list(idp = cv.Grid[i, 'Beta']))
    ### PERFORM LOOCV ###
    crossval <- gstat.cv(idw,
                         nmax = cv.Grid[i, 'Neighbors'],
                         beta = v.Grid[i, 'Beta'],
                         debug.level = 0)
    cv.Grid[i, 'RMSE'] <- RMSE(crossval$residual)
    if (verbose){
      print(paste('Function call', i, 'out of',  nrow(cv.Grid)))
      print(paste('Evaluating beta =',
                  cv.Grid[i, 'Beta'],
                  'and neighbors =',
                  cv.Grid[i, 'Neighbors']))
      print(paste('RMSE=', RMSE(crossval$residual)))
    }
  }

  ### GET BEST PARAMTER VALUES ###
  idx.min <- which.min(cv.Grid$RMSE)
  best.Beta <- cv.Grid$Beta[idx.min]
  best.Neighbors <- cv.Grid$Neighbors[idx.min]
  min.RMSE <- cv.Grid$RMSE[idx.min]

  ### BUILD IDW MODEL BASED ON BEST PARAMTER VALUES ###
  idw.best <- gstat(formula = stat.formula,
                    data = spatialDF,
                    nmax = best.Neighbors,
                    set = list(idp = best.Beta))

  ### PREPARE EVALUATION GRID ###
  # grid.evalGrid  <- expand.grid(x = seq(from = round(extent.evalGrid@xmin),
  #                                       to = round(extent.evalGrid@xmax),
  #                                       by = evalGridSize),
  #                               y = seq(from = round(extent.evalGrid@ymin),
  #                                       to = round(extent.evalGrid@ymax),
  #                                       by = evalGridSize))
  #
  #
  # coordinates(grid.evalGrid) <- ~x + y
  # proj4string(grid.evalGrid) <- proj4string(spatialDF)
  grid.evalGrid = grid.DE
  gridded(grid.evalGrid) <- TRUE
  idw.best$data$var1$data = spTransform(idw.best$data$var1$data,crs)

  ### INTERPOLATE VALUES FOR EVALUATION GRID USING THE BEST MODEL ###
  idw.best.predict <- predict(object = idw.best$data,
                              newdata = grid.evalGrid,
                              debug.level = 0)

  ### RETURN RESULTS AND OBJECTS ###
  return(list('idwBestModel' = idw.best,
              'idwBestRaster' = idw.best.predict,
              'bestBeta' = best.Beta,
              'bestNeighbors' = best.Neighbors,
              'bestRMSE' = min.RMSE,
              'gridCV' = cv.Grid))
}

RMSE <- function(residuals){
  sqrt(sum((residuals)^2)/length(residuals))
}



download.dwd.data <- function(df,
                              res = "monthly",
                              var = "kl",
                              per = "historical",
                              RUN = TRUE, verbose = TRUE,
                              forceDownload = FALSE){

  ##############################################
  ### FUNCTION TO DOWNLOAD DATA FROM THE DWD ###
  ##############################################
  library(rdwd)
  library(tidyverse)
  library(lubridate)

  if (RUN){

    ## COLUMNS TO KEEP
    df.column.names <- c("Stations_id", "Stationshoehe",
                         "geoBreite", "geoLaenge", "Stationsname",
                         "Bundesland")
    df.result <- df[, df.column.names]

    ## RENAME COLUMNS
    df.column.names.english <- c("Station_id", "Altitude", "Lat", "Lon",
                                 "Name", "Federal.State")
    colnames(df.result) <- df.column.names.english

    ## EMPTY COLUMNS TO STORE RESULTS
    df.result['Rainfall'] <- NA
    df.result['Temperature'] <- NA

    ## LOOP THROUGH STATIONS ##
    for (j in 1:length(df$Stations_id)){
      if (verbose){
        ## Write progress information to console
        print(paste('Weather station', df[j, 'Stationsname'], '--',
                    j, 'out of', length(df$Stations_id)))
      }

      ## DOWNLOAD STATION DATA ##
      ## for detailed instructions type vignette("rdwd") into the console
      tdir <- tempdir()
      link <- selectDWD(id = df$Stations_id[10],
                        res = res,
                        var = var,
                        per = per)
      file <- dataDWD(link,
                      read = F,
                      dir = tdir,
                      quiet = TRUE,
                      force = forceDownload)
      clim <- readDWD(file)
      clim['Date'] = ymd(clim$MESS_DATUM)
      clim = clim[year(clim$MESS_DATUM) >= 2000,]
      ## LOOP THROUGH PARAMETERS ##
      params = c(quo(TMK), quo(RSK))
      for (i in 1:length(params)){
        # initial dataframe
        ## CONTROL FLOW ##
        ## if there is no data for the period 81-10 continue
          # if there is data, make sure we have data for each year

            ## IF SO BUILD OUTPUT DATA FRAME ##
            ## Temperature

              result <- round(mean(df_$day.mean, na.rm = T),1)
              df.result[j, 'Temperature'] <- result

              ## Rainfall
              result <- round(mean(df_$day.sum, na.rm = T))
              df.result[j, 'Rainfall'] <- result

}

    }
    #df.result <- df.result[complete.cases(df.result),]
    write.csv(x = df.result,
              file = 'dwd_data_1981-2010.csv',
              row.names = F)
    return(df.result)
  }else{
    ## DOWNLOAD PREPROCESSED DATA ##
    url <- "https://userpage.fu-berlin.de/soga/300/30100_data_sets/"
    df.result <- read.csv(paste0(url,'dwd_data_1981-2010.csv'))
    print('########################################################')
    print('### NOTE THAT YOUR ARE DOWNLOADING PREPROCESSED DATA ###')
    print('### THE ARGUMENTS TO THE FUNCTION CALL ARE DISMISSED ###')
    print('########################################################')
    return(df.result)
  }
}

download.dwd.data_2 <- function(df,
                              res = "daily",
                              var = "kl",
                              per = "h",
                              fromDate = "20000131",
                              toDate = "20201231",
                              crs = NULL,
                              verbose = TRUE,
                              forceDownload = FALSE){

  ##############################################
  ### FUNCTION TO DOWNLOAD DATA FROM THE DWD ###
  ##############################################

  if (is.null(crs))
    crs = raster::crs("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=GRS80 +units=m +no_defs")



  ## COLUMNS TO KEEP
  df.column.names <- c("Stations_id", "Stationshoehe",
                       "geoBreite", "geoLaenge", "Stationsname",
                       "Bundesland")
  df.result <- df[, df.column.names]

  ## RENAME COLUMNS
  df.column.names.english <- c("Station_id", "Altitude", "Lat", "Lon",
                               "Name", "Federal.State")
  colnames(df.result) <- df.column.names.english

  ## EMPTY COLUMNS TO STORE RESULTS
  df.result['Rainfall'] <- NA
  df.result['Temperature'] <- NA

  ## LOOP THROUGH STATIONS ##
  for (j in 1:length(df$Stations_id)){
    if (verbose){
      ## Write progress information to console
      print(paste('Weather station', df[j, 'Stationsname'], '--',
                  j, 'out of', length(df$Stations_id)))
    }

    ## DOWNLOAD STATION DATA ##
    ## for detailed instructions type vignette("rdwd") into the console
    tdir <- envrmt$path_GhcnDaily
    link <- selectDWD(id = df$Stations_id[j],
                      res = res,
                      var = var,
                      per = "r")
    #url <- gsub (pattern = "ftp:",replacement = "HTTPS:",link)
    #url <- gsub (pattern = fromDate,replacement = toDate,url)

    file <- dataDWD(link,
                    read = F,
                    dir = tdir,
                    quiet = TRUE,
                    force = F)
fail=0
    if (!file.exists(file))
   fail = try(downloader::download(url = url,destfile=file))
 if (fail==0){
  #try(  downloader::download(url = url,destfile=file))
    clim <- readDWD(file)
    clim['Date'] = ymd(clim$MESS_DATUM_BEGINN)

    ## LOOP THROUGH PARAMETERS ##
    params = c(quo(TNK), quo(RSK))
    for (i in 1:length(params)){
      # initial dataframe
      df_ <- clim %>%
        # generate one column and populate it with the year
        mutate(day = day(Date)) %>%
        # group the data by the new columns
        group_by(day) %>%
        # select only years between 1981 and 2010
        filter(year(Date) >= 2000 ) %>%
        summarise(
          # calculate the monthly sum for each year
          day.sum = sum(!!params[[i]], na.rm = T),
          # calculate the monthly mean for each year
          day.mean = mean(!!params[[i]], na.rm = T)) %>%
        # Add column with the number of years in record
        mutate(day.count = n())

      ## CONTROL FLOW ##
      ## if there is no data for the period 81-10 continue
      if(identical(df_$day.count, integer(0))){
        next
      }else{
        # if there is data, make sure we have data for each year
        if (mean(df_$day.count, na.rm = T) == 30){

          ## IF SO BUILD OUTPUT DATA FRAME ##
          ## Temperature
          if (any(grepl(pattern = 'T',x = params[[i]]))==TRUE){
            result <- round(mean(df_$day.mean, na.rm = T),1)
            df.result[j, 'Temperature'] <- result
          }else{
            ## Rainfall
            result <- round(mean(df_$day.sum, na.rm = T))
            df.result[j, 'Rainfall'] <- result
          }
        }
      }
    }
  }}
  df.result <- df.result[complete.cases(df.result),]
  write.csv(x = df.result,
            file = paste0(envrmt$path_data_lev1,"/",fromDate,toDate,'dwd_data.csv',collapse = "_"),
            row.names = F)
  return(df.result)
}

