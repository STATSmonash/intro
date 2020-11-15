### Data summary functions

fsum_grid<-function(x,do.extra=TRUE,do.autocor=TRUE,autocor.lag=6) {
  outn<-list(c("table"))
  x<-as.matrix(x)
  #T<- nrow(x)
  T<-apply(!is.na(x),2,sum)
  out<-T
  out<-rbind( out, apply( x ,2, fsum_moment, mom=1,cent=FALSE))
  out <-rbind(out, sqrt(apply( x ,2, fsum_moment, mom=2,dfadj=1)))
  out <-rbind(out, apply( x ,2, fsum_moment, mom=3,norm=TRUE))
  out <-rbind(out, apply( x ,2, fsum_moment, mom=4,norm=TRUE))
  out<-rbind( out , T/6*out[4,]^2 + T/24*(out[5,]-3)^2 )
  pskew<-2*(1- pnorm(abs(out[4,]),0,sqrt(6/T)))
  pkurt<-2*(1-pnorm(abs(out[5,]-3),0,sqrt(24/T)))
  pjb<- 1-pchisq(out[6,],2)
  out<-rbind(out , pskew , pkurt , pjb)
  
  out.names<-c("n"  , "mean" , "sd" , "skew" , "kurt" , "JarqBera" , "pval skew" , "pval kurt" , "pval JB" )
  
  
  if(do.autocor){
    cor.lag<-autocor.lag
    cor.tests<-as.matrix(apply(x,2,function(i){unlist(fsum_auto_cor_test(i,cor.lag,2:cor.lag))}))
    out<- rbind(as.matrix(out),as.matrix(cor.tests[c(1,2,3),]))
    out.names<-c(out.names, "autocor(ac1)","pval ac1",paste0("pval ac1:",cor.lag,"") )
  }

  
  dimnames(out)<-list(out.names,dimnames(x)[[2]])
  
  if(do.extra){
    out.extra<- apply(x,2,function(i){c(min(i,na.rm=TRUE),max(i,na.rm=TRUE),as.vector(quantile(i,c(.025,.05,.25,.5,.75,.95,.975),na.rm=TRUE)))})
    rownames(out.extra)<-c("min", "max", "q.025","q.05", "q.25", "q.5", "q.75", "q.95", "q.975")
    out<-rbind(out,out.extra)
  }
  
  blurb<-paste0("The first three rows are sample size, mean and standard deviation."
                , "  Skewness and kurtosis are relative to a normal distribution (which has skewness 0 and kurtosis 3)."
                , "  JarqBera is a JB test statistic (a weighted average of kurtosis and skewness) used to assess normality."
                , "  The p-values for skewness, kurtosis and JB are for tests of normality (low values are evidence against normality)."
                , "  The autocor(ac1) row is the first lag autocorrelation and is followed by p-values for testing the null hypothesis of zero autocorrelation (based on the Ljung-Box Q-test for one and then up to number of lags shown)."
                , "  The minimum, maximum and quantile (q.) statistics follow in the remaining rows.")
  
  outn$table<-round(out , 9)
  outn$blurb<-blurb
  outn
}


fsum_auto_cor<-function(x,lag){
  x<-x[!is.na(x)]
  #lag<-5
  xlen<-length(x)
  
  lag.index<-(lag+1):xlen
  index<-1:(xlen-lag)
  xmu<-mean(x)
  
  covK<-(1/(xlen-1))*sum((x[index]-xmu)*(x[lag.index]-xmu))
  cov0<-var(x)
  corK<-covK/cov0
  
  out<-list("covK"=covK,"corK"=corK)
  out
  
}

fsum_auto_cor_test<-function(x,lag,lags.test=NULL){
  if(is.null(lags.test)){lags.test<-1:lag}
  cors<-unlist(lapply(1:lag,function(i){fsum_auto_cor(x,i)$corK}))
  cor.test<-cors^2*length(x)
  
  pval1<-1-pchisq(sum(cor.test[1]),1)
  pval.1tolag<-1-pchisq(sum(cor.test),length(cor.test))
  pval.lags.limit<-1-pchisq(sum(cor.test[lags.test]),length(lags.test))
  autocor1<-cors[1]
  out<-list("autocor1"=autocor1,"pval1"=pval1,"pval.1tolag"=pval.1tolag,"pval.lags.limit"=pval.lags.limit)
  out
}


fsum_moment<-function(x, mom , norm=FALSE , cent=TRUE , dfadj=0) { 
  x<-x[!is.na(x)]
  n<-length(x)
  if (cent ==TRUE)   { out<- sum(  (x-mean(x))^mom )/( n-dfadj) }
  else   		{ out<- sum(  (x)^mom )/( n-dfadj) }
  if (norm==TRUE)	{ sigma<- sqrt( sum( (x-mean(x))^2 )/n) ; out <- out/sigma^mom }
  out }



