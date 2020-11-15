#clear_win<-function(){ x<-dev.list() ; if(length(x)>=1) {for (i in 1:length(x)){ dev.off(x[i]) }} }

ian_wil<-function(x){
  
  #x<-yd-xd
  x.abs<-abs(x)
  n<-length(x)
  
  x.sign<-sign(x)
  x.rank<-rank(x.abs)
  
  w.plus<-sum(x.rank[x.sign==1])
  out<-pnorm( w.plus , n*(n+1)/4 , sqrt(n*(n+1)*(2*n+1)/24) )            
  out
}

### resample from multinorm

ian_rmultnorm <- function(n, mu, vmat,sim.seed=NULL )
{
  
  if(is.null(sim.seed)==FALSE){set.seed(sim.seed)}
  
  p <- ncol(vmat)
  vs <- svd(vmat)  			##svd = Splus CHOLESKY DECOMPOSITION
  vsqrt <- t(vs$v %*% (t(vs$u) * sqrt(vs$d)))      
  #vsqrt <-chol(vmat)
  ans <- matrix(rnorm(n * p), nrow = n) %*% vsqrt
  ans <- sweep(ans, 2, mu, "+")
  ans
}

### Data summary
ian_auto_cor<-function(x,lag){
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

ian_auto_cor_test<-function(x,lag,lags.test=NULL){
  if(is.null(lags.test)){lags.test<-1:lag}
  cors<-unlist(lapply(1:lag,function(i){ian_auto_cor(x,i)$corK}))
  cor.test<-cors^2*length(x)
  
  pval1<-1-pchisq(sum(cor.test[1]),1)
  pval.1tolag<-1-pchisq(sum(cor.test),length(cor.test))
  pval.lags.limit<-1-pchisq(sum(cor.test[lags.test]),length(lags.test))
  autocor1<-cors[1]
  out<-list("autocor1"=autocor1,"pval1"=pval1,"pval.1tolag"=pval.1tolag,"pval.lags.limit"=pval.lags.limit)
  out
}


ian.moment<-function(x, mom , norm=FALSE , cent=TRUE , dfadj=0) { 
  x<-x[!is.na(x)]
  n<-length(x)
  if (cent ==TRUE)   { out<- sum(  (x-mean(x))^mom )/( n-dfadj) }
  else   		{ out<- sum(  (x)^mom )/( n-dfadj) }
  if (norm==TRUE)	{ sigma<- sqrt( sum( (x-mean(x))^2 )/n) ; out <- out/sigma^mom }
  out }


ian_sum<-function(x,do.ar=FALSE,do.extra=TRUE,do.autocor=TRUE,autocor.lag=6) {
  outn<-list(c("table","kurt","skew","pkurt","pskew"))
  x<-as.matrix(x)
  #T<- nrow(x)
  T<-apply(!is.na(x),2,sum)
  out<-T
  out<-rbind( out, apply( x ,2, ian.moment, mom=1,cent=FALSE))
  out <-rbind(out, sqrt(apply( x ,2, ian.moment, mom=2,dfadj=1)))
  out <-rbind(out, apply( x ,2, ian.moment, mom=3,norm=TRUE))
  out <-rbind(out, apply( x ,2, ian.moment, mom=4,norm=TRUE))
  out<-rbind( out , T/6*out[4,]^2 + T/24*(out[5,]-3)^2 )
  pskew<-pnorm(out[4,],0,sqrt(6/T))
  pkurt<- pnorm(out[5,],3,sqrt(24/T))
  pjb<- 1-pchisq(out[6,],2)
  out<-rbind(out , pskew , pkurt , pjb)
  
  out.names<-c("n"  , "mean" , "sd" , "skew" , "kurt" , "JarqBera" , "pval skew" , "pval kurt" , "pval JB" )
  
  
  if (do.ar) {
    ar.temp<-NULL;ar.order<-NULL
    #for (i in 1:ncol(x)){ar.temp<-ar(x[,i]) ; ar.order<-c(ar.order,ar.temp$order)}
    for (i in 1:ncol(x)){if(out[3,i]!=0){ar.temp<-ar(x[,i]) ; ar.order<-c(ar.order,ar.temp$order)}  else {ar.order<-c(ar.order,0)}}
    out<- rbind(out,ar.order)
    out.names<-c(out.names, "AR(order)")
  }
  
  if(do.autocor){
    cor.lag<-autocor.lag
    cor.tests<-as.matrix(apply(x,2,function(i){unlist(ian_auto_cor_test(i,cor.lag,2:cor.lag))}))
    out<- rbind(as.matrix(out),as.matrix(cor.tests[c(1,2,3),]))
    out.names<-c(out.names, "autocor(ac1)","pval ac1",paste0("pval ac1:",cor.lag,"") )
  }
  #out<-rbind( out, as.numeric(c(simplify2array(apply( x ,2, ar), higher=TRUE)[1,])) )  ## AR auto select by AIC model order
  
  
  dimnames(out)<-list(out.names,dimnames(x)[[2]])
  
  if(do.extra){
    out.extra<- apply(x,2,function(i){c(min(i,na.rm=TRUE),max(i,na.rm=TRUE),as.vector(quantile(i,c(.025,.05,.25,.5,.75,.95,.975),na.rm=TRUE)))})
    rownames(out.extra)<-c("min", "max", "q.025","q.05", "q.25", "q.5", "q.75", "q.95", "q.975")
    out<-rbind(out,out.extra)
  }
  
  outn$table<-round(out , 9)
  outn
}



