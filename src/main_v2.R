#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]

#devtools::install_github("envima/envimaR")
library(envimaR)
library(rprojroot)
appendProjectDirList = c("data/data_lev0/GhcnDaily","data/data_lev0/GhcnMonthly")
root_folder = find_rstudio_root_file()

source(file.path(root_folder, "src/functions/000_setup.R"))

crs = raster::crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
epsg=3035
res=500
startYear = 2000

# get data
source(file.path(envrmt$path_src,"prepare germany_data.R"))
cVar = "UPM"
##------------------ day data set

dat_list = sort(as.character(unique(cVar.sp$MESS_DATUM)))[1:1620]



z=1
for (currentDate in dat_list) {
    cVar.sp.day = cVar.sp[as.character(unique(cVar.sp$MESS_DATUM)) == as.Date(currentDate),]
    cVar.sp.day <- spatialEco::sp.na.omit(cVar.sp.day,cVar)
    cd= substr(currentDate,1,10)
    cVar.sp.day
    cat(cd, " date ",z, " von ",length(dat_list),"\n")
    # tune idw model
    # set parameter
    # # neighbors = length(cVar.sp.day)-1
    # #
    # # my_IDW <- cv.IDW(spatialDF = cVar.sp.day,
    # #                  stat.formula = formula(as.formula(paste(cVar, "~ 1"))),
    # #                  seqBeta = seq(from = 1.5, to = 2.7, 0.3),
    # #                  seqNeighbors = seq(from = 10, to = neighbors / 4, 3),
    # #                  evalGridSize = 500,
    # #                  evalRaster = grid.DE,
    # #                  verbose = TRUE)
    # #
    # # gridCV <- my_IDW$gridCV
    # # head(gridCV[with(gridCV, order(RMSE)),],10)
    # #
    # # ggplot(gridCV, aes(Neighbors, Beta)) +
    # #   geom_tile(aes(fill = RMSE), colour = "black") +
    # #   scale_fill_gradient(low = "steelblue", high = "orange") +
    # #   theme_bw() +
    # #   ggtitle('Parameter search space')
    # #
    # daily_temp = mask(raster(my_IDW[['idwBestRaster']]),DE.states.sp) # aus dem optimierungslauf

    # mapview(daily_temp) +
    #    mapview(DE.states.sp,alpha.regions=0)+
    #    mapview(cVar.sp.day[,1], cex = 0.5, color = "red")

    #---------------------------------------------------------------------------------------------------------------------

    # ev <- variogram(cVar~1, cVar.sp.day, cloud = TRUE)
    # plot(ev)

    # ev <- variogram(cVar~1, cVar.sp.day)
    # plot(ev)


    # vm <- vgm(psill = 13.14087, model = "Exp", range = 248719.8, nugget= 0)
    # plot(ev,vm)
    #
    # vm.exp <- fit.variogram(ev, vgm("Exp"))
    # vm.gau <- fit.variogram(ev, vgm("Gau"))
    # vm.sph <- fit.variogram(ev, vgm("Sph"))
    # vm.pen <- fit.variogram(ev, vgm("Pen"))
    # plot(ev, vm.exp, main = 'Exponential variogram model')
    # plot(ev, vm.gau, main = 'Gauss variogram model')
    # plot(ev, vm.sph, main = 'Spherical variogram model')
    # plot(ev, vm.pen, main = 'Spherical variogram model')


    vm.auto = autofitVariogram(formula = as.formula(paste(cVar, "~ 1")),
                               input_data = cVar.sp.day)
    # plot(vm.auto)

    # anisotropy
    # alpha <- (0:3)*45
    # alpha
    # ev.anis <- variogram(cVar~1, cVar.sp.day, alpha = alpha)
    # plot(ev.anis)
    # plot(ev.anis, vm.gau)
    #
    # vm.anis <-  vgm(psill = 7.6086173,
     #                 model = "Exp",
    #                 range = 131529.2,
    #                 nugget = 0.9792447,
    #                 anis = c(0, 1))
    # plot(ev.anis,vm.anis)

    # rain.OK.LOOCV.auto <- krige.cv(formula = as.formula(paste(cVar, "~Stationshoehe")),
    #                               locations = cVar.sp.day,
    #                               model = vm.auto$var_model)
    #
    # RMSE(residuals = rain.OK.LOOCV.auto@data$residual)

    # bubble(rain.OK.LOOCV.auto,
    #        "residual",
    #        main = "Ordinary Kriging rainfall: LOOCV residuals")

    cVar.sp.day = spTransform(cVar.sp.day,
                              crs(srtm.germany.spdf))

    tmax.pred <- krige(formula = as.formula(paste(cVar, "~Stationshoehe")),
                       locations = cVar.sp.day,
                       newdata = srtm.germany.spdf,
                       model = vm.auto$var_model,
                       debug.level=0)

    writeRaster(raster(tmax.pred, layer=1, values=TRUE),paste0(envrmt$path_data_lev1,"/",cd,"_",cVar,".tif"),overwrite=TRUE,options="COMPRESS=LZW")
    z=z+1
}


# pal.nr = ceiling(max(range(tmax.pred@data$var1.pred)) - min(range(tmax.pred@data$var1.pred)))
# spplot(tmax.pred['var1.pred'],
#        main = 'Predicted daily Max Temperature [C] ',
#        #at = seq(min.val, max.val, 1),
#        col.regions = rev(viridis(pal.nr)))
