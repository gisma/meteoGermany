gemeinden_temp <- function(l)
{
  #rnr = int[[lki]] # msf row number
  # if(length(rnr)<1)
  # {
  #   warning("No data available for Landkreis ", lki, ": ", code[lki], call.=FALSE)
  #   out <- data.frame(NA,NA)[FALSE,]
  #   colnames(out) <- c("MESS_DATUM", as.character(code[lki]))
  #   return(out)
  # }
  urls <- selectDWD(id = lki[l], res="daily", var="kl", per=type, outvec=TRUE)
  clims <- dataDWD(urls, varnames=FALSE, dir=envrmt$path_GhcnDaily)
  maxtemp <- clims[c("STATIONS_ID","MESS_DATUM","RSK","SDK","NM","VPM","PM","TMK","UPM","TXK","TNK","TGK")]
  return(maxtemp)

}

