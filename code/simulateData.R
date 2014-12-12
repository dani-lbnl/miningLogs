#---------------------------------------------------------------------------------
#This code generates synthetic log files
#
# Created by Dani Ushizima
# Last modified: 09/12/2014
#---------------------------------------------------------------------------------

#Configuration of synthetic usage: you may want to change these values
nsamps = 50;
ndays = 10
nsoft = 5;
path = "/Users/ushizima/Documents/prog/git/miningLogs/code/"
whereToWrite = paste(path,'syntheticLogs_nsamp',nsamps,'_laptop_2014.txt',sep='');

#Possible values
users = c('Beth','Cyrus','Nick','Dav', 'Jey', 'Kyle','Yu', 'Zhao', 'Katy', 'Fatma', 'Justin', 'Kartik', 'Falk', 'Daniel','Dani');
dates = seq(from=as.Date("2014/01/01"), to=as.Date("2014/09/23"), by=ndays) 
softwares = LETTERS[1:nsoft]
nusers = length(users)

#Creates the synthetic data, picking possible values randomly
dflogs = data.frame(date=dates[round(runif(nsamps,min=1,max=ndays))], 
                    user=users[round(runif(nsamps,min=1,max=nusers))], 
                    activity=softwares[round(runif(nsamps,min=1,max=nsoft))])

#Sort dflogs by date after all it's collected by date
dflogs = dflogs[with(dflogs,order(date,activity)),]
write.table(dflogs, file=whereToWrite, sep = ' ', row.names = F, col.names = F, quote = F)
print(paste('File saved at:',whereToWrite));