# The script is frame work to split the climate data into different catagory. 
# Input data
# Need Edit !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     df:       A data set that contains a variable values with date. First three columns
#               should be year, month and day values and rest of the columns could be N number of stations. 
#               (dimension of data frame is [X, N + 3] where X is the timeseries number and N is the station number ) 
#               (dimension of data frame is [X, N + 3] where X is the timeseries number and N is the station number ) 
#
#     per:      The percentage of the data that intended to be splited based on year value. 
#               Year(int) values should be reside in first column.  

# Required library
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(automap)


# Required functions 
source('Functions/subMonth.R')
source('Functions/probThreshold4Temp.R')


# ---------------------------------------------  Main Function  -------------------------------------------------------- #
probRaster <- function(list.tmax, list.tmin, begin.month, begin.day, end.month, end.day, 
                       varThreshold.max, varThreshold.min,
                       raster_grid, location_df, force.choice=NA) {
  output.list <- {}
  # Number of slice
  for (slice_no in 1:2){
    # selecting the slice for tmax
    dfSelect = list.tmax[[slice_no]]
    dfSelSorted.Tmax <- subMonth(dfSelect, begin.month, begin.day, end.month, end.day)
    
    # selecting the slice for tmin
    dfSelect = list.tmin[[slice_no]] 
    dfSelSorted.Tmin <- subMonth(dfSelect, begin.month, begin.day, end.month, end.day)
    
    
    output.list[[slice_no]] <- probThreshold4Temp( dfSelSorted.Tmax, dfSelSorted.Tmin, varThreshold.max, varThreshold.min )
  }
  
  output.ras <-  data.frame( output.list[[1]]$probCDF , output.list[[2]]$probCDF )  
  
  # Finding the common choice
  if ( output.list[[1]]$choice != output.list[[2]]$choice) {
    temp.list <- output.list[[2]]
    output.ras[,2] = temp.list[[paste0("prob", output.list[[1]]$choice)]]
  } 
  
  if(is.na(force.choice)==F){
    temp.list <- output.list[[1]]
    output.ras[,1] = temp.list[[paste0("prob", force.choice)]]
    temp.list <- output.list[[2]]
    output.ras[,2] = temp.list[[paste0("prob", force.choice)]]
    output.list[[1]]$choice = force.choice
  }
  
  tryCatch({
    if(sum(output.ras[,1])==0 | sum(output.ras[,2])==0 ) {
      print(output.list[[1]]$choice)
      print(output.list[[1]]$count_Tmax_cond)
      print(output.list[[1]]$count_Tmin_cond)
      print(output.list[[2]]$count_Tmax_cond)
      print(output.list[[2]]$count_Tmin_cond)
      print(output.ras[,1])
      print(output.ras[,2])
      stop("Threshold out of range")
    }
  
    
    # adjusting the grid 
    spts <- rasterToPoints(raster_grid, spatial = T)
    llprj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    llpts <- spTransform(spts, CRS(llprj))
    dummy.grid <- as.data.frame(llpts)
    gridded(dummy.grid) = ~x+y
    
    raster.list = {} 
    for ( raster_indx in 1:2 ){
      # Loading required rasters
      varAll <- location_df[,c(9,8,7)]
      varAll[,3] <- output.ras[,raster_indx]
      # print(varAll[,3])
      colnames(varAll)[1] <- 'x'
      colnames(varAll)[2] <- 'y'
      colnames(varAll)[3] <- 'values'
      coordinates(varAll) <- ~x+y
      # print(raster_indx)
      
      # Krining the surface
      intras0 <- autoKrige(values ~ 1, varAll, dummy.grid)
      intras0 <- raster(intras0[[1]],  values=T)
      e <- extent(raster_grid)
      s <- raster(e, nrows=raster_grid@nrows, ncols=raster_grid@ncols, crs=raster_grid@crs)
      r1 <- resample(intras0, s, method = 'ngb')
      intras <- mask(r1, raster_grid)
      projection(intras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      
      raster.list[[raster_indx]] <- list("values"=values(intras), "output.threshold"=output.list, 
                                         "selected_variable"=output.list[[1]]$choice, 
                                         "selected_PDF"=output.ras)
      print(paste0('selected choice:',output.list[[1]]$choice ))
      
    }
    return(raster.list)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
