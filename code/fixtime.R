#Convert date from weird log files to a format that can be used with googleVis
# Created by Dani Ushizima - dani.lbnl@gmail.com
# Last Modified: 09/14/2014
#---------------------------------------------------------------------------

# Wed_Jun_04_2014_08:40 blableuser bleblasftw 
fixtime <-function(oneRecord){
  require(lubridate)
  parts = unlist(strsplit(oneRecord,"_")); # these will generate a character vector with the parts of the data: you want the 2,3,4 elements
  oneDate <- paste(parts[2],parts[3],parts[4],sep='_')
  oneDate <- mdy(oneDate,quiet=T) #return a POSIXct, otherwise na
  fixtime = as.character(oneDate) #if you need to return as character for text mining
}