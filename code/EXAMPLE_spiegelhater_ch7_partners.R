cat("\014")  # clean console window
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv) # remove all variables and start again :)

# get file from Spiegalhalter's book website
fname<-"https://raw.githubusercontent.com/dspiegel29/ArtofStatistics/master/07-1-2-3-distributions-of-partners%2Bbootstrap/7-1%20datasnatch.csv"
new_csv_name<-"partners_raw.csv"
download.file(fname, destfile = new_csv_name) #, method = "curl")
temp_raw<-read.csv(file="partners_raw.csv",check.names = FALSE)  # upload from clean CSV file
temp<-temp_raw[temp_raw$size=="N=760",]
write.table(temp_raw)

temp # not in the right format

# so recreate the original dataset from the count table
x_short<-data.frame(partners=temp$partners,count=as.numeric(round(temp$count*760,0)))
x<-data.frame(partners=unlist(apply(x_short,1,function(i){rep(i[1],i[2])})),row.names=NULL)

# check out the data (just like the results of real survey)
dim(x) # should say 760 rows and 1 column

par(mfrow=c(1,2)) # makes two plots, side by side
hbins<-seq(-1,51,1) # bins for histogram (to match book - a bar for each integer (zeros as well))
hist(x$partners,breaks=hbins,main=NA,xlab="number of partners") # should look like the book page 193, last chart.
qqnorm(x$partners,main="qqplot with N(0,1)");qqline(x$partners);
title(main="the straighter the line the more normal the data",cex.main=.65,line=.75,font.main=1)


###################################################
###  bootstrap means
###################################################

## main idea is to resample from the rows (1:nrow(x)) of x many times ---> then use these index numbers to point to the data and make new bootstrap-samples.
nboot<-1000 # number of bootstrap samples to take
nsamp<-25

# example:  
sample(1:nrow(x),nsamp,replace = TRUE)

# example with nboot columns ...
boot_index<-matrix(sample(1:nrow(x),nsamp*nboot,replace = TRUE),ncol=nboot)
dim(boot_index) # should be nsamp rows (of index numbers) and nboot columns
boot_index[,1:2] # first two columns

boot_samples<-apply(boot_index,2,function(i){x[i,1]})
dim(boot_samples) # should be nsamp rows (of randomly selected partner data) and nboot columns
boot_samples[,1:2]# first two columns

# now take means and plot
par(mfrow=c(1,2)) # makes two plots, side by side
boot_means<-apply(boot_samples,2,mean)
hbins<-c(seq(-1,max(30,max(boot_means)+1),1))
hist(boot_means,breaks=hbins,include.lowest=TRUE,main=paste0("N=",nsamp),ylim=c(0, nboot*.6),xlab=paste0(nboot," bootstrap means"))
lines(c(mean(x[,1]),mean(x[,1])),c(0, nboot*.6),lty=2) # line for true mean
title(main=paste0("true mean=",mean(x[,1])),cex.main=.65,line=.75,font.main=1)

qqnorm(boot_means,main="qqplot with N(0,1)");qqline(boot_means);
title(main="the straighter the line the more normal the data",cex.main=.65,line=.75,font.main=1)


