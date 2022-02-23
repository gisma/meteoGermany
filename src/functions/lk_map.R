lk_temp <- function(lki)
{
  rnr = int[[lki]] # msf row number
  if(length(rnr)<1)
  {
    warning("No data available for Landkreis ", lki, ": ", lk$name[lki], call.=FALSE)
    out <- data.frame(NA,NA)[FALSE,]
    colnames(out) <- c("MESS_DATUM", as.character(lk$name[lki]))
    return(out)
  }
  urls <- selectDWD(id = m[rnr, "Stations_id"], res="daily", var="kl", per="h", outvec=TRUE)
  clims <- dataDWD(urls, varnames=FALSE, dir=envrmt$path_GhcnDaily)
  if(length(urls)==1)
  {maxtempmean <- clims$TXK
  maxtemp <- clims[c("MESS_DATUM", "TXK")]
  } else
  {
    maxtemp <- lapply(seq_along(clims), function(n)
    {
      out <- clims[[n]][c("MESS_DATUM", "TXK")]
      colnames(out)[2] <- names(clims)[n] # no duplicate names
      out
    })
    maxtemp <- Reduce(function(...) merge(..., by="MESS_DATUM",all=TRUE), maxtemp)
    maxtempmean <- rowMeans(maxtemp[,-1], na.rm=TRUE) # check also with median, variation is huge!
  }
  out <- data.frame(maxtemp[,1], maxtempmean)
  colnames(out) <- c("MESS_DATUM", as.character(lk$name[lki]))
  return(out)
}

