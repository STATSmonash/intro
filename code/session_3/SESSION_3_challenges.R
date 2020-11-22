cat("\014")  # clean console window
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv) # remove all variables and start again :)

# load all function files in function folder (files begining with "f" only)
function_files<-paste0("source('functions//",list.files(path="functions//",pattern="^f.*.R$"),"', encoding='utf-8')")
for(i in function_files){ eval(parse(text=i)) }
colour_p<-c("#E768AD") # pink for some charts!


#################################################################################################### 
# challenge 1
#################################################################################################### 
getwd() # csv file must be in there (or otherwise specify full filepath)


# read in raw file (assumes it is the working directory), formated with columns set by a tab
temp<-read.csv(file="DATA_academic_burnout.csv",check.names = FALSE,stringsAsFactors =TRUE , na.strings = c("NA"," ",""))  # upload from clean file tab delimited (\t)
View(temp)
str(temp)

x<-temp

#################################################################################################### 
# challenge 2
#################################################################################################### 

fsum_mini<-function(x){
  xna<-is.na(x) # which elements are missing
  x<-x[!xna] # only keep numbers!
  out<-c(length(x),mean(x),sd(x),max(x)) # whatever you want
  names(out)<-c("n","mean","sd","max") # gives nice names to the elements
  out # last line send output back (very important!)
}

xdo<-cbind(x$burnout,x$work_hours)
apply(xdo,2,fsum_mini)



#################################################################################################### 
# challenge 3
#################################################################################################### 

hist(x$burnout,main="burnout") # histogram with titel
# also look at xlab, ylab and the title function

?tiff
# need to open file, make the plot, close the file

tiff(filename = "hist_test.tif", width = 480, height = 480)
  hist(x$burnout,main="burnout") # histogram with titel
dev.off()

# this writes the file out to your working directory


#################################################################################################### 
# challenge 4
#################################################################################################### 

plot(x$age,x$burnout)

# a sneak peak ahead and beyond (a linear regression model)...
summary(lm(x$burnout~x$age+x$work_hours))  # now plot burnout versus hours ...






#################################################################################################### 
# challenge 5
#################################################################################################### 

hist(x$MAS_c) # check for skewness
qqnorm(x$MAS_c) # plot quantiles of data versus what would be expected from a N(0,1) distribution
qqline(x$MAS_c) # add a straight line (if the points follow this the data look are roughly normal)

qqplot(x$burnout,x$MAS_c)

# little function to draw line through interquartile range (analogous to qqline for qqnorm plot)
fqqline<-function(x,y){
  line_slope<-(quantile(y,.75,na.rm=TRUE)-quantile(y,.25,na.rm=TRUE))/ (quantile(x,.75,na.rm=TRUE)-quantile(x,.25,na.rm=TRUE))
  lines(x,quantile(y,.25,na.rm=TRUE)-quantile(x,.25,na.rm=TRUE)*line_slope+x*line_slope,col=2)
}

fqqline(x$burnout,x$MAS_c)

# # test out line drawing function
# xx<-rnorm(100); yy<-rexp(100,10)#xx<-rnorm(100); yy<-rnorm(1000,10,20)
# qqplot(xx,yy) ;fqqline(xx,yy)



#################################################################################################### 
# challenge 6
#################################################################################################### 

cor(x$burnout,x$work_hours) # error --- check NAs

?cor

cor(x$burnout,x$work_hours,use="pairwise.complete.obs") 


plot(x$work_hours,x$burnout)

# quick check with linear regression model
mini_reg<-lm(x$burnout~x$work_hours)  # now plot burnout versus hours ...
summary(mini_reg) # r-squared is cor()^2
lines(mini_reg$model$`x$work_hours`,predict(mini_reg)) # predicted values of y for each x


#################################################################################################### 
# challenge 7
#################################################################################################### 

# see the file called EXAMPLE_anscombe.R from session_1_2


#################################################################################################### 
# challenge 8
#################################################################################################### 
#q1
# i am going to use burnout and work_hours.  these are columns 1 and 5 of x.
#q2
plot(x$burnout[x$resilience_simple=="resilience"],x$work_hours[x$resilience_simple=="resilience"],main=NA)
title(main="hours vs burnout for those with resilience",cex.main=1)

# now repeat for "no resilience"


#q3
xsplit<-split(x,x$resilience_simple) # splits the whole dataframe
lapply(xsplit,function(i){apply(i[,c(1,5)],2,fsum_mini)}) #fsum_mini must be loaded from earlier ..





#################################################################################################### 
# challenge 9
#################################################################################################### 


# choose a factor column
data_box<-split(x$burnout,x$resilience_simple) # split a numeric column by a factor column

colour_p<-c("#E768AD") # pink for some charts!
stripchart(data_box, jitter = .1 , method = "jitter",vertical = TRUE,col=0,xlab=NULL,cex.axis=1,axes=TRUE)
boxplot(data_box,outline=FALSE,add=TRUE,names=NA,axes=FALSE,boxwex=0.25)
stripchart(data_box,jitter = .1 , method = "jitter",pch=16,cex=1.5, col=adjustcolor(colour_p, alpha.f = 0.3),vertical = TRUE,add=TRUE,axes=FALSE)
title(ylab="burnout",main="resilience_simple")



#################################################################################################### 
# challenge 10
#################################################################################################### 

#q1
# for numeric variables i am going to use burnout and work_hours.  these are columns 1 and 5 of x.
# for the factor i choose resilience_simple
xsplit<-split(x[,c(1,5)],x$resilience_simple) # splits the columns of x by the factor 

#q2
?t.test

t.test(xsplit[[1]]$burnout,xsplit[[2]]$burnout)
t.test(xsplit[[1]]$work_hours,xsplit[[2]]$work_hours)

#q3
# look at the confidence intervals and p-values





#################################################################################################### 
# challenge 11
#################################################################################################### 

#q1
xsplit<-split(x$age,x$resilience_simple) # splits the columns of x by the factor 



#q2
par(mfrow=c(1,3)) # split the chart screen into 3 bits
hist(xsplit[[1]],main="burnout",xlab=names(xsplit)[1])
hist(xsplit[[2]],main="burnout",xlab=names(xsplit)[2])

qqplot(xsplit[[1]],xsplit[[2]])
fqqline(xsplit[[1]],xsplit[[2]]) # if you have loaded this earlier in the R session

# and copy code from above for fsum_mini


#q3
?wilcox.test
# the function works with rank data ... 
wilcox.test(xsplit[[1]],xsplit[[2]])

xsplit_ranks<-split(data.frame(rank=rank(x$age)),x$resilience_simple)
sapply(xsplit_ranks,fsum_mini)

#q4
t.test(xsplit[[1]],xsplit[[2]])












