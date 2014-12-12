#Analysis code to mine log files
# Created by Dani Ushizima - dani.lbnl@gmail.com
# Last Modified: 09/14/2014
#---------------------------------------------------------------------------
rm(list = ls()) #clean up env

#---File input HERE-----------------------------------------
path = '/Users/ushizima/Documents/prog/git/miningLogs/code/';
outputPath = paste(path,'results/')
filename="syntheticLogs_nsamp50_laptop_2014.txt";
bSyntheticData = T


#---Packages and functions
require('plyr')
require('ggplot2')
require('lubridate')

#source('~/Dropbox/fixtime.R')
source(paste(path, 'plotGraphs_v2.R', sep=''));

#this parsing is highly dependent on the data generator
str = strsplit(paste(path, filename, sep=''),'/');
str = strsplit(unlist(str)[length(str[[1]])],'_');
machine = unlist(str)[length(str[[1]])-1];
year = strsplit(unlist(str)[length(str[[1]])],'.txt')[[1]];
if(is.na(year))
  year = '2014'    

path = paste(path,filename,sep='')
f = file.info(path)
print(paste('File contains: ',f$size/(1024)^2,'MB',sep=''))
#initialization
cutoff = 0;

ptime <- system.time({
  dfLogFile = dfLogFile = read.delim(path,sep = " ",header=FALSE,stringsAsFactors=F,na.strings='NA',fill=T,blank.lines.skip=T,skipNul=T);
  names(dfLogFile)<-c("date","user","sftw") #labeling columns to easy code understanding
  dstart=dfLogFile$date[2]
  dend=dfLogFile$date[length(dfLogFile$date)-1]
  moduleLogFileDuration = as.duration(new_interval(dstart,dend));
  
  #transform date-time into date only for frequency analysis
  if(!bSyntheticData)
    dfLogFile$date = substr(dfLogFile$date,5,15);
  #-----------------------------------------------------------------------------
  #(1) Aggregate values using sftw as unique
  dfSftwFreqRaw = count(dfLogFile, vars='sftw');
  ind = which(dfSftwFreqRaw$freq>cutoff)
  path = paste(outputPath, 'reportSftwUsageRaw_',machine,year,'.txt',sep='')
  write.table(dfSftwFreqRaw[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfSftwFreqRaw,substr(path,1,nchar(path)-4),machine,year,cutoff); #(filename, machine, year, xlabel)
  #-----------------------------------------------------------------------------
  #(2) Specifies a particular software given a keyword
  nUsesOfR = which(substr(dfSftwFreqRaw$sftw,1,1)=="R"); #assuming that the only sftw starting with R is R
  tt = sum(dfSftwFreqRaw[nUsesOfR,2]);
  print(paste("R was recorded through <module load> calls",tt,"times in ",path))
  #-----------------------------------------------------------------------------
  #(3) List software usage counting use ONCE a day (prevent multiple counts when using parallel jobs)
  # see Queries
  
  #-----------------------------------------------------------------------------
  #Query 1: Sftw popularity: for each software, count how many days was it used, ex: x was used 40 days in a year
  #this list uses 3x more memory
  listBySftw <- split(dfLogFile, list(dfLogFile$sftw)) #splits data frame into list, each item contains all records for a certain software
  listBySftwDayUnique = mclapply(listBySftw,function(x) unique(x$date)) #??? what about the user?
  listBySftwDayUniqueCount = mclapply(listBySftwDayUnique,length); #length returns the #unique_days
  
  dfSftwCounter = data.frame(sftw=names(listBySftwDayUniqueCount), ndays=unlist(listBySftwDayUniqueCount));
  ind = which(dfSftwCounter$ndays>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportSftwUsageInDays_',machine,year,'.txt',sep='')
  write.table(dfSftwCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfSftwCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff); 
  #-----------------------------------------------------------------------------
  #Query 2: Daily usage: for each date, count how many sftw was used, ex: on Jan11, 50 different sftwr were used
  listByDate <- split(dfLogFile, list(dfLogFile$date)) #splits data frame into list, each item contains all records for a certain software
  listByDateUniqueSftw = mclapply(listByDate,function(x) unique(x$sftw)) #??? what about the user?
  listByDateUniqueSftwCount = mclapply(listByDateUniqueSftw,length); #length returns the #unique_days
  ndays = length(listByDateUniqueSftw) #must be the same as in moduleLogFileDuration
  
  dfDateCounter = data.frame(date=names(listByDateUniqueSftwCount), nsftw=unlist(listByDateUniqueSftwCount));
  ind = which(dfDateCounter$nsftw>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportDailyUsageOfSftws_',machine, year,'.txt',sep='')
  write.table(dfDateCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfDateCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff);
  #-----------------------------------------------------------------------------
  #Query 3: Daily usage: for each date, count how many different users ran some software, ex: on Jan11, 50 different users were on the machine
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
  #Query 4: for each software, count sftw usage per day, with each different user for a single sftw counting +1 ex: s1 was used by 50 users in a day
  # remember that some users do not exist, i.e. login not registered
  #list of list requires rapply = recursive apply
  listOfList = mclapply(listBySftw, function(x) split(x,list(x$user))) #return a list of list (sftw->user->dates)
  #you want the length of each list of list, which counts s1d1u1 and s1d1u2 as different instances
  listByDateUniqueUser = mclapply(listOfList,function(x) mclapply(x,function(y) unique(y$date)))
  listByDateUniqueUserCount = mclapply(listByDateUniqueUser, function(x) length(unlist(x)))
  #notice: you are recycling dfDateCounter variable
  dfDateCounter = data.frame(date=names(listByDateUniqueUserCount), nsftw=unlist(listByDateUniqueUserCount));
  ind = which(dfDateCounter$nsftw>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportDailyUsageBySftwEachUser_',machine,year,'.txt',sep='')
  write.table(dfDateCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfDateCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff);
  
  #-----------------------------------------------------------------------------
  #Query 5: count the number of unique users of each package
  listOfList = mclapply(listBySftw, function(x) split(x,list(x$user))) #return a list of list (sftw->user->dates)
  #you want the length of each list of list, which counts s1d1u1 and s1d1u2 as different instances
  listNUserPerPkg = mclapply(listOfList,function(x) length(x))
  
  #notice: you are recycling dfDateCounter variable
  dfDateCounter = data.frame(sftw=names(listNUserPerPkg), freq=unlist(listNUserPerPkg));
  ind = which(dfDateCounter$freq>cutoff) #eliminate bogus ??? TODO: double check
  path = paste(outputPath, 'reportUniqueUsersEachPackage_',machine,year,'.txt',sep='')
  write.table(dfDateCounter[ind,], file=path, sep = ' ', row.names = F, col.names = T, quote = F)
  plotGraphs_v2(dfDateCounter[ind,],substr(path,1,nchar(path)-4),machine,year,cutoff);
},gc=T) #end timing
print(paste('File takes: ',ptime[1],'s. to load, process and write report',sep=''))
path = paste(outputPath, 'time_',unlist(str)[1],unlist(str)[2],'.txt',sep='')
write(paste('ModuleLogFile duration:',moduleLogFileDuration,'.\n It took: ',ptime[1],' to process it.',sep=''),path)
  