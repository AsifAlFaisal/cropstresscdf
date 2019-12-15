# The script is frame work to split the climate data into different catagory. 
# Input data
#
#     df:       A data set that contains a variable values with date. First three columns
#               should be year, month and day values and rest of the columns could be N number of stations. 
#               (dimension of data frame is [X, N + 3] where X is the timeseries number and N is the station number ) 
#               (dimension of data frame is [X, N + 3] where X is the timeseries number and N is the station number ) 
#
#     per:      The percentage of the data that intended to be splited based on year value. 
#               Year(int) values should be reside in first column.  


# ---------------------------------------------  Main Function  -------------------------------------------------------- #
splitFun <-  function(df, per) {
  
  ## Gettign first and last year values
  first.year.value <- df[1,1]
  last.year.value <- df[length(df[,1]),1]
  
  ## determining the first split  
  year.break <- as.integer (per/100 *(last.year.value - first.year.value)) + first.year.value
  
  # position of year break 
  all.postion <- which(df[,1]==year.break)
  break.postion <- max(all.postion)
  print(year.break)
  
  df.1s <- df[1:break.postion, ]
  df.2s <- df[(break.postion+1):length(df[,1]), ]
  list.split <- list(df.1s, df.2s)
  
  return(list.split)
}