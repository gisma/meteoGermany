correct_daytime = function(fn){
  for (f in fn){
    current = terra::rast(f)
    dt=suncalc::getSunlightTimes(date = as.Date(substr(basename(f),1,10)), lat = 51.0, lon = 9.0, tz = "UTC")
    td=dt$sunset-dt$sunrise
    maxDaylight= ceiling(as.numeric(unlist(stringr::str_split(td,"Time difference of "))))
    m <- c(-999, 0,0, maxDaylight,99999,maxDaylight)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    current <- terra::classify(current, rclmat, include.lowest=TRUE)
    names(current) = xfun::sans_ext(basename(f))
    writeRaster(current, f,gdal=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
  }
}
