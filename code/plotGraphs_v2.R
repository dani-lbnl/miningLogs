#
# Function to plot graphs 
# Created: Aug 2014 by Dani Ushizima 
# Modified: 09/02/14
#
plotGraphs_v2 <-function(a, filename, machine, year,cutoff){ 
  #dev.off() if you start getting weird bugs, run this command
  #require('ggplot2')
  
  
  title = paste('Top50 on ',machine,' during ',year,': freq>',cutoff,sep='');
  xlabel = names(a)[1]
  ylabel = names(a)[2]
  names(a)=c('v1','v2')
  b = subset(a,v2>cutoff,select=cbind(v1,v2)) 
  sorted=sort(b$v2,decreasing=T,index.return=T)
  ind = sorted$ix[1:(min(50,length(sorted$ix)))]
  b = b[ind,]
  ind = which(rownames(b)!="") #remove bad logs with no software name registered
  b = b[ind,]
  
  savedGG = ggplot(b)
  savedGG = savedGG + geom_bar(aes(y=v2, x=reorder(v1,-v2)),stat="identity", fill=b$v2)  
  savedGG = savedGG+xlab(xlabel)+ylab(ylabel)+ggtitle(title)+theme(axis.text.x=element_text(size=rel(0.75),angle=60,hjust=1))
  savedGG = savedGG + geom_text(aes(label = v2, y = v2+1, x=v1), size = 2)
  ggsave(paste(filename,".png",sep=''), plot = savedGG, width = 8, height = 5)
  
  # #savedGG + geom_bar(aes(x=nuses, y=sftw)) + coord_flip() + scale_y_continuous('') + scale_x_discrete('')
  # 
  # kkk
  # c <- ggplot(c)
  # c + geom_bar(stat="bin") 
  # 
  # 
  # 
  # ggplot(hdata, aes(y=sftw,x=reorder(nuses,nuses)))
  # +geom_bar()
  #        + geom_bar(aes(x=sftw,y=nuses), fill="blue") +
  #   coord_flip() + 
  #   # reduce the font size of the y-axis tick labels
  #   theme(axis.text.y=element_text(size=rel(0.8)))
}