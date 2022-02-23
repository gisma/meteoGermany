#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]
#'

library(envimaR)
library(rprojroot)
appendpackagesToLoad = c("lubridate","rdwd","berryFunctions")
appendProjectDirList = c("data/data_lev0/GhcnDaily","data/data_lev0/GhcnMonthly")


root_folder = find_rstudio_root_file()

source(file.path(root_folder, "src/functions/000_setup.R"))

#---------------------------------------------------------------------


crs = raster::crs("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=GRS80 +units=m +no_defs")


# ## download all station data
# for (id in df.station$Stations_id){
# link <- selectDWD(id = id, res="daily", var="kl", per="hr")
# file <- dataDWD(link, read=FALSE, dir=envrmt$path_GhcnDaily, force=NA, overwrite=TRUE)
# }
#
#

# Select daily climate data:
data("metaIndex")
m <- metaIndex
m <- m[m$res=="daily" & m$var=="kl" & (m$per=="historical") & m$hasfile, ]

#drop=   c(1380,3537)

# # Exclusion of station Kaltennordheim due to a corrupt file
# m <- m[m$Stationsname != 'Kaltennordheim',]
# m=  m[!m$Stations_id %in% drop, ]

# Transform into spatial object:
msf <- sf::st_as_sf(m, coords=c("geoLaenge", "geoBreite"), crs=4326)
#msf_25832 = st_transform(msf,crs = 25832)

# # Read district shapefile, see link above:
# gm <- sf::st_read("data/data_lev0/VG250_GEM.shp", quiet=TRUE)
# lk <- sf::st_read("data/data_lev0/covid-19-germany-landkreise.shp", quiet=TRUE)
# gm = st_transform(gm,crs = 4326)
# gm = st_make_valid(gm) %>% st_cast("MULTIPOLYGON")

# intersections: list with msf rownumbers for each district:
# int <- sf::st_intersects(gm, msf)
# plot.new()
# plot(gm[,"NUTS"], reset=FALSE)
# colPoints("geoLaenge", "geoBreite", "Stationshoehe", data=m, add=T, legend=F,pch=16, col="yellow", cex=.5)
# axis(1, line=-1); axis(2, line=-1, las=1)
# points(m[int[[3]], c("geoLaenge", "geoBreite")], pch=16, col="red", cex=1.8)

#
# tm_shape(lk) + tm_polygons(col = "bez") + tm_shape(msf) + tm_dots() +
#   tm_layout(legend.outside.position = "right",
#             legend.outside = T,
#             panel.label.height=0.6,
#             panel.label.size=0.6,
#             panel.labels = c("Verfügbare Wetterstationen/Gemeinden nach Ländern")) +
#   tm_grid()
#

######
# Select daily climate data:
data("metaIndex")
m <- metaIndex
m <- m[m$res=="daily" & m$var=="kl" & (m$per=="historical") & m$hasfile, ]

# Transform into spatial object:
msf <- sf::st_as_sf(m, coords=c("geoLaenge", "geoBreite"), crs=4326)
# extract station ids for station after 2000
lki = msf[year(msf$von_datum) >= 2000 ,]$Stations_id
rainLK <- pbapply::pblapply(1:length(lki), gemeinden_temp)
gm_tempMax = do.call(rbind,rainLK)
names(stations)[1] = "STATIONS_ID"

# create subset from msf according to the data
stations = msf[year(msf$von_datum) >= 2000 ,]
merge = merge(stations,gm_tempMax)
st_write(merge,paste0(envrmt$path_data_lev0,"/daily_climate.gpkg"))
saveRDS(merge,paste0(envrmt$path_data_lev0,"/daily_climate.rds"))

tmap_mode("view")
qtm(merge,symbols.col = "TXK")

