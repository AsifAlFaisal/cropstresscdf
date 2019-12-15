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
subMonth <- function( dfSelect, begin_month, begin_day, end_month, end_day) {
  
  dfSelSorted <- dfSelect[ ( dfSelect[,2] == begin_month & dfSelect[,3] >= begin_day ) | ( dfSelect[,2] == end_month & dfSelect[,3] <= end_day ) , ] 
  return(dfSelSorted)
}