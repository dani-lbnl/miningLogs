#Analysis code to mine log files
# Created by Dani Ushizima - dani.lbnl@gmail.com
# Last Modified: 09/14/2014
#---------------------------------------------------------------------------
rm(list = ls()) #clean up env

#---File input HERE-----------------------------------------
inputPath = '/Users/ushizima/Documents/prog/git/miningLogs/code/';
outputPath = paste(inputPath,'results/',sep='')
filename="syntheticLogs_nsamp50_laptop_2014.txt";
bSyntheticData = T


#---Packages and functions
require('plyr')
require('ggplot2')
require('lubridate')
require('multicore')

#source('~/Dropbox/fixtime.R')
source(paste(inputPath, 'plotGraphs_v2.R', sep=''));

#this parsing is highly dependent on the data generator
str = strsplit(paste(inputPath, filename, sep=''),'/');
str = strsplit(unlist(str)[length(str[[1]])],'_');
machine = unlist(str)[length(str[[1]])-1];
year = strsplit(unlist(str)[length(str[[1]])],'.txt')[[1]];
if(is.na(year))
  year = '2014'    

path = paste(inputPath,filename,sep='')
f = file.info(path)
print(paste('File contains: ',f$size/(1024)^2,'MB',sep=''))
#initialization
cutoff = 0; #when dealing with large amounts of data, you may want ignore low frequencies

ptime <- system.time({
  dir.create(outputPath, showWarnings = F, recursive = FALSE, mode = "0777")
  dfLogFile = read.delim(path,sep = " ",header=FALSE,stringsAsFactors=F,na.strings='NA',fill=T,blank.lines.skip=T,skipNul=T);
  names(dfLogFile)<-c("date","user","activity") #labeling columns to easy code understanding
  dstart=dfLogFile$date[2]
  dend=dfLogFile$date[length(dfLogFile$date)-1]
  moduleLogFileDuration = as.duration(new_interval(dstart,dend));
  
  #transform date-time into date only for frequency analysis
  if(!bSyntheticData)
    dfLogFile$date = substr(dfLogFile$date,5,15);
  #-----------------------------------------------------------------------------
  #(1) Aggregate values using activity as unique
  dfactvtFreqRaw = count(dfLogFile, vars='activity');
  ind = which(dfactvtFreqRaw$freq>cutoff)
  path = paste(outputPath, 'reportactvtUsageRaw_',machine,year,'.txt',sep='')
  write.table(dfactvtFreqRaw[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfactvtFreqRaw,substr(path,1,nchar(path)-4),machine,year,cutoff); 

#(filename, machine, year, xlabel)
  #-----------------------------------------------------------------------------
  #(2) Specifies a particular software given a keyword
  nUsesOfR = which(substr(dfactvtFreqRaw$actvt,1,1)=="R"); #assuming that the only actvt starting with R is R
  tt = sum(dfactvtFreqRaw[nUsesOfR,2]);
  print(paste("R was recorded through <module load> calls",tt,"times in ",path))
  #-----------------------------------------------------------------------------
  #(3) List software usage counting use ONCE a day (prevent multiple counts when using parallel jobs)
  
  #-----------------------------------------------------------------------------
  #Query 1: actvt popularity: for each software, count how many days was it used, ex: x was used 40 days in a year
  #this list uses 3x more memory
  listByactvt <- split(dfLogFile, list(dfLogFile$activity)) #splits data frame into list, each item contains all records for a certain software
  listByactvtDayUnique = mclapply(listByactvt,function(x) unique(x$date)) #??? what about the user?
  listByactvtDayUniqueCount = mclapply(listByactvtDayUnique,length); #length returns the #unique_days
  
  dfactvtCounter = data.frame(activity=names(listByactvtDayUniqueCount), ndays=unlist(listByactvtDayUniqueCount));
  ind = which(dfactvtCounter$ndays>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportactvtUsageInDays_',machine,year,'.txt',sep='')
  write.table(dfactvtCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfactvtCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff); 
  #-----------------------------------------------------------------------------
  #Query 2: Daily usage: for each date, count how many actvt was used, ex: on Jan11, 50 different actvtr were used
  listByDate <- split(dfLogFile, list(dfLogFile$date)) #splits data frame into list, each item contains all records for a certain software
  listByDateUniqueactvt = mclapply(listByDate,function(x) unique(x$activity)) #??? what about the user?
  listByDateUniqueactvtCount = mclapply(listByDateUniqueactvt,length); #length returns the #unique_days
  ndays = length(listByDateUniqueactvt) #must be the same as in moduleLogFileDuration
  
  dfDateCounter = data.frame(date=names(listByDateUniqueactvtCount), nactvt=unlist(listByDateUniqueactvtCount));
  ind = which(dfDateCounter$nactvt>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportDailyUsageOfactvts_',machine, year,'.txt',sep='')
  write.table(dfDateCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfDateCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff);
  #-----------------------------------------------------------------------------
  #Query 3: Daily usage: for each date, count how many different users performed some activity, ex: on Jan11, 50 different users were doing something
  #reuses variable from last query
  listByDateUniqueUser = mclapply(listByDate,function(x) unique(x$user)) #??? what about the user?
  listByDateUniqueUserCount = mclapply(listByDateUniqueUser,length); #length returns the #unique_days
  #notice: you are recycling dfDateCounter variable
  dfDateCounter = data.frame(date=names(listByDateUniqueUserCount), nuser=unlist(listByDateUniqueUserCount));
  ind = which(dfDateCounter$nuser>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportDailyUsageByUser_',machine, year,'.txt',sep='')
  write.table(dfDateCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfDateCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff);
  #-----------------------------------------------------------------------------
  #Query 4: for each activity, increment 1 for each day a user performed it
  
  listOfList = mclapply(listByactvt, function(x) split(x,list(x$user))) #return a list of list (actvt->user->dates)
  #you want the length of each list of list, which counts a1d1u1 and a1d1u2 as different instances
  listByDateUniqueUser = mclapply(listOfList,function(x) mclapply(x,function(y) unique(y$date)))
  listByDateUniqueUserCount = mclapply(listByDateUniqueUser, function(x) length(unlist(x)))
  #notice: you are reusing dfDateCounter variable
  dfDateCounter = data.frame(actv=names(listByDateUniqueUserCount), freq=unlist(listByDateUniqueUserCount));
  ind = which(dfDateCounter$freq>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportDailyUsageByactvtUniqueUser_',machine,year,'.txt',sep='')
  write.table(dfDateCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfDateCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff);
  
  #-----------------------------------------------------------------------------
  #Query 5: count the number of unique users performing each activity
  listOfList = mclapply(listByactvt, function(x) split(x,list(x$user))) #return a list of list (activity->user->dates)
  #you want the length of each list of list, which counts s1d1u1 and s1d1u2 as different instances
  listNUserPerPkg = mclapply(listOfList,function(x) length(x))
  
  #notice: you are recycling dfDateCounter variable
  dfDateCounter = data.frame(activity=names(listNUserPerPkg), freq=unlist(listNUserPerPkg));
  ind = which(dfDateCounter$freq>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportUniqueUsersEachPackage_',machine,year,'.txt',sep='')
  write.table(dfDateCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfDateCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff);
},gc=T) #end timing

#How long does it take to work this out?-----------------------------------
print(paste('File takes: ',ptime[1],'s. to load, process and write report',sep=''))
path = paste(outputPath, 'time_',unlist(str)[1],unlist(str)[2],'.txt',sep='')
write(paste('ModuleLogFile duration:',moduleLogFileDuration,'.\n It took: ',ptime[1],' to process it.',sep=''),path)
plot(table(dfLogFile$user,dfLogFile$activity),main='What are these people doing?')
  