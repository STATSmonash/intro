cat("\014")  # clean console window
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv) # remove all variables and start again :)
colour_p<-c("#E768AD")

x<-anscombe[,1:4]
y<-anscombe[,5:8]
xy<-lapply(1:4,function(i){data.frame(dataset=LETTERS[i],x=x[,i],y=y[,i])})
names(xy)<-LETTERS[1:length(xy)]


# write out some files with the base data
#for(i in 1:length(xy)){write.csv(xy[[i]],paste0("anscombe_",names(xy)[i],".CSV"),row.names = FALSE)}
combo<-do.call(rbind, xy)
write.csv(combo,paste0("anscombe_data.CSV"),row.names = FALSE)

fsum_mini<-function(x){
    out<-c(mean(x),sd(x))
    names(out)<-c("mean","sd")
    out
}

# plot the data and fit linear regressions
freg_mini<-function(x,y,num){
  reg<-lm(y~x)
  plot(x,y,pch=21,col=0,bg=adjustcolor(colour_p,alpha.f=.75),cex=1.75)
  title(main=paste0("data set ",num),font.main=2,cex.main=1)
  lines(x,reg$fitted.values,col=adjustcolor("grey",alpha.f=.5),lwd=2)
  list(reg=reg,cor=cor(x,y))
}

windows(10,10)
par(mfrow=c(2,2))
xy_reg<-lapply(1:length(xy),function(i){freg_mini(xy[[i]]$x,xy[[i]]$y,names(xy)[i])})


# summarise the data
x_sum<-apply(x,2,fsum_mini); rownames(x_sum)<-paste0("x ",rownames(x_sum))
y_sum<-apply(y,2,fsum_mini); rownames(y_sum)<-paste0("y ",rownames(y_sum))
cor_pearson<-sapply(xy,function(i){cor(i$x,i$y,method="pearson")})
cor_spearman<-sapply(xy,function(i){cor(i$x,i$y,method="spearman")})
r_squared<-sapply(xy_reg,function(i){summary(i$reg)$r.squared})

out_sum<-rbind(x_sum,y_sum,cor_pearson,cor_spearman,r_squared)
colnames(out_sum)<-names(xy)
out_sum


# jit<-apply(xy[[4]],2,function(i){jitter(i,.1)})
# cor(apply(xy[[4]],2,rank),method="pearson")
# cor(apply(jit,2,rank),method="pearson")




