
group_size<-20 # assumed sample size per grou
mean_acne<-5 # true expected pimple count without any adverse jelly bean effect
col_list<-c("purple","brown","pink","blue","teal","salmon","red","turquoise","magenta","yellow","grey","tan","cyan","GREEN","mauve","beige","lilac","black","peach","orange")
#col_list<-colors() # list of pretend jelly bean colours ; col_list<-col_list[-c(grep("^gray",col_list),grep("^grey",col_list))]

ntests<-length(col_list)
ntests

# null hypothesis value of mean (true in every case for simulations)
m0<-mean_acne

# function to generate random pimple count (poisson count random variables)
facne_count<-function(n,ave){rpois(n,ave)}

x<-lapply(1:length(col_list),function(i){facne_count(group_size,mean_acne)})
names(x)<-col_list
#hist(unlist(x)) # raw pimple histogram
hist(sapply(x,mean),main="mean acne count per colour") # histogram of mean counts per colour

pvals<-lapply(x,function(i){t.test(i,mu=m0)$p.value})
x_p<-data.frame(colour=col_list,p=unlist(pvals),row.names=NULL)
hist(x_p$p)

# sorted list of p-values
head(x_p[order(x_p$p),])

sum(x_p$p<.05)/ntests # proportion of p-values below .05

# bonferroni adjustment (divide p-value cut-off by the number of tests)
.05/ntests # new cut-off
sum(x_p$p<.05/ntests) # number of p-values below bonferroni cut-off




# fquicksim<-function(cutoff){
#   x<-lapply(1:length(col_list),function(i){facne_count(group_size,mean_acne)})
#   pvals<-unlist(lapply(x,function(i){t.test(i,mu=m0)$p.value}))
#   sum(pvals<cutoff)
# }
# 
# 
# nsim<-10000
# rerun<-unlist(lapply(1:nsim,function(i){fquicksim(.05/ntests)}))
# sum(rerun)


