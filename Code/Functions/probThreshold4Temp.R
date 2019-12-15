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


# ---------------------------------------------  Main Function  -------------------------------------------------------- #
probThreshold4Temp <- function(dfTmax, dfTmin, varThreshold.max, varThreshold.min ,missing.value.threshold=0.5 ) {
  # Initiaalizing the required vectorS 
  varProbCDF.max<-NA
  varProbCDF.min<-NA
  varMax.tmax <- NA
  varMin.tmax <- NA
  varMean.tmax <- NA
  varMax.tmin <- NA
  varMin.tmin <- NA
  varMean.tmin <- NA
  
  
  
  
  # ******************** Maximum temperature selection ******************** 
  dfSelTmax <- dfTmax[ , colSums(is.na(dfTmax)) <= missing.value.threshold * (length(dfTmax[,1]))]
  
  # Finding common CDF 
  for (i in 1:(length(dfSelTmax[1,])-3)){
    varSel <- dfSelTmax[,i+3]
    varSel <- na.omit(varSel)
    varMax.tmax <- c(varMax.tmax, max(varSel))
    varMin.tmax <- c(varMin.tmax, min(varSel))
    varMean.tmax <- c(varMean.tmax, mean(varSel))
    varSelCDF <- ecdf(varSel)
    varProbCDF.max <-c(varProbCDF.max , (1 - varSelCDF(varThreshold.max)))
  }
  
  # removing the extra NA values
  varProbCDF.max <-  varProbCDF.max[-1]
  varMax.tmax <- varMax.tmax[-1]
  varMin.tmax <- varMin.tmax[-1]
  varMean.tmax <- varMean.tmax[-1]
  
  # ******************** Minimum temperature selection ********************
  dfSelTmin <- dfTmin[ , colSums(is.na(dfTmin)) <= missing.value.threshold * (length(dfTmin[,1]))]
  
  # Finding common CDF 
  for (i in 1:(length(dfSelTmin[1,])-3)){
    varSel <- dfSelTmin[,i+3]
    varSel <- na.omit(varSel)
    varSelCDF <- ecdf(varSel)
    varMax.tmin <- c(varMax.tmin, max(varSel))
    varMin.tmin <- c(varMin.tmin, min(varSel))
    varMean.tmin <- c(varMean.tmin, mean(varSel))
    varProbCDF.min <-c(varProbCDF.min , (varSelCDF(varThreshold.min)))
  }
  
  # removing the extra NA values
  varProbCDF.min <-  varProbCDF.min[-1]
  varMax.tmin <- varMax.tmin[-1]
  varMin.tmin <- varMin.tmin[-1]
  varMean.tmin <- varMean.tmin[-1]
  
  # Average values of all stations
  varMean.all.st.tmax <- mean( varMax.tmax )
  varMean.all.st.tmin <- mean( varMin.tmin )
  
  # Count of temperature threshold 
  varCount.threshold.tmax <- sum( varMean.tmax > varThreshold.max )
  varCount.threshold.tmin <- sum( varMean.tmin < varThreshold.min )
  print(varCount.threshold.tmax)
  print(varCount.threshold.tmin)
  print(varThreshold.max)
  print(varThreshold.min)
  print(varMean.tmax)
  
  # Selection of requried variables 
  varProbCDF <- varProbCDF.max
  choice <- "Upper"
  if ( varCount.threshold.tmax > varCount.threshold.tmin ) {
    if (varCount.threshold.tmin >=3){
    varProbCDF <- varProbCDF.min
    choice <- "Lower"
    }
  }  else if (varCount.threshold.tmax == 0){
    varProbCDF <- varProbCDF.min
    choice <- "Lower"
  }

  
  list.return = {}
  list.return[[1]] <- varProbCDF
  list.return[[2]] <- varProbCDF.max
  list.return[[3]] <- varProbCDF.min
  list.return[[4]] <- list( "Max"= varMax.tmax,
                            "Min"= varMin.tmax,
                            "Mean" = varMean.tmax)
  list.return[[5]] <- list( "Max"= varMax.tmin,
                            "Min"= varMin.tmin,
                            "Mean" = varMean.tmin)
  list.return[[6]] <- varCount.threshold.tmax
  list.return[[7]] <- varCount.threshold.tmin
  list.return[[8]] <- choice
  list.return[[9]] <- colnames(dfSelTmin )[4:length(dfSelTmin [1,])]
  names(list.return) <- c("probCDF", "probUpper", "probLower", "Tmax.stat", "Tmin.stat", 
                          "count_Tmax_cond", "count_Tmin_cond", "choice", "Station_name")
  print(choice)
  
  return(list.return)
}
