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


# --------------------------- #
#   Initialization section    #
# --------------------------- #

# Required library
library(raster)
library(rgdal)
library(maptools)
library(sp)
#library(gstat)
library(automap)


# Required functions 
source('Functions/splitFun.R')
source('Functions/subMonth.R')
source('Functions/probThreshold4Temp.R')


# ---------------------------------------------  Main Function  -------------------------------------------------------- #
probRasterWhole <- function(dfTmax.Whole, dfTmin.Whole, list.tmax, list.tmin, begin.month, begin.day, end.month, end.day,
                            varThreshold.max, varThreshold.min,
                            raster_grid, location_df, force.choice=NA) {
  
  output.list <- {}
  # For the whole time period
  dfSelect = dfTmax.Whole
  dfSelSorted.Tmax <- subMonth(dfSelect, begin.month, begin.day, end.month, end.day)
  
  # selecting the slice for tmin
  dfSelect = dfTmin.Whole
  dfSelSorted.Tmin <- subMonth(dfSelect, begin.month, begin.day, end.month, end.day)
  
  output.list[[1]] <- probThreshold4Temp( dfSelSorted.Tmax, dfSelSorted.Tmin, varThreshold.max, varThreshold.min )
  
  # Selecting output raster 
  output.ras <-  data.frame( output.list[[1]]$probUpper , output.list[[1]]$probLower ) 
  
  
  # Number of slice
  for (slice_no in 1:2){
    # selecting the slice for tmax
    dfSelect = list.tmax[[slice_no]]
    dfSelSorted.Tmax <- subMonth(dfSelect, begin.month, begin.day, end.month, end.day)
    
    # selecting the slice for tmin
    dfSelect = list.tmin[[slice_no]] 
    dfSelSorted.Tmin <- subMonth(dfSelect, begin.month, begin.day, end.month, end.day)
    
    
    output.list[[slice_no+1]] <- probThreshold4Temp( dfSelSorted.Tmax, dfSelSorted.Tmin, varThreshold.max, varThreshold.min )
  }
  
  # Selecting output raster 
  output.ras <-  cbind( output.ras , output.list[[2]]$probUpper , output.list[[2]]$probLower, output.list[[3]]$probUpper , output.list[[3]]$probLower ) 
  output.select <- list(output.list[[1]], output.list[[1]],
                        output.list[[2]], output.list[[2]],
                        output.list[[3]], output.list[[3]])
  
  # # Only for testing
  # raster_grid = dummy.ps 
  # location_df = varlocation
  
  # adjusting the grid
  spts <- rasterToPoints(raster_grid, spatial = T)
  llprj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  llpts <- spTransform(spts, CRS(llprj))
  dummy.grid <- as.data.frame(llpts)
  gridded(dummy.grid) = ~x+y
  
  
  raster.list = {} 
  for ( raster_indx in 1:6 ){
    # Loading required rasters
    varAll <- location_df[,c(9,8,7)]
    varAll[,3] <- output.ras[,raster_indx]
    colnames(varAll)[1] <- 'x'
    colnames(varAll)[2] <- 'y'
    colnames(varAll)[3] <- 'values'
    coordinates(varAll) <- ~x+y
    
    # Krining the data points to a surface
    intras0 <- autoKrige(values ~ 1, varAll, dummy.grid)
    intras0 <- raster(intras0[[1]],  values=T)
    e <- extent(raster_grid)
    s <- raster(e, nrows=raster_grid@nrows, ncols=raster_grid@ncols, crs=raster_grid@crs)
    r1 <- resample(intras0, s, method = 'ngb')
    intras <- mask(r1, raster_grid)
    projection(intras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    raster.list[[raster_indx]] <- list("values"=values(intras), "output.list"=output.select[[raster_indx]] )
    print(paste0('Probable choice to make:',output.select[[raster_indx]]$choice ))
  }
  return(raster.list)
  
}


