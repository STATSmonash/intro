fresidual_check<-function(x){
  # residual checks for residuals coming in as a list ... 
  # x<-c(res_list,list(overall=as.matrix(unlist(res_list))))  
  # x<-list(unlist(res_list))
  
  # plots
  chart_out<-NULL
  par(mfrow=c(1,length(x)))
  lapply(1:length(x),function(i){qqnorm(x[[i]],main=names(x)[i]);qqline(x[[i]])})
  chart_out<-list(chart_out,recordPlot())
  
  # summary stats
  out_res_sum<-sapply(x,function(i){fsum_grid(i,autocor.lag=2)$table}); rownames(out_res_sum)<-rownames(fsum_grid(x[[1]],autocor.lag=2)$table)
  out_res_sqr_sum<-sapply(x,function(i){fsum_grid(i^2,autocor.lag=2)$table}); rownames(out_res_sqr_sum)<-rownames(fsum_grid(x[[1]]^2,autocor.lag=2)$table)
  get_row1<-c("n","mean","sd","pval JB","pval ac1")
  get_row2<-c("pval ac1")
  
  out1<-as.matrix(out_res_sum[rownames(out_res_sum)%in%get_row1,])
  out2<-matrix(out_res_sqr_sum[rownames(out_res_sum)%in%get_row2,],nrow=length(get_row2))
  if(length(get_row2)==1){rownames(out2)<-paste(get_row2,"(squared)")} else {rownames(out2)<-paste(rownames(out2),"(res^2)")}
  
  out_sum<-rbind(out1,out2)
  
  stats_blurb<-paste0("The rows are: number of observations (n); mean; standard deviation;"
                    , " a p-value for a Jarque-Bera test of normality (pval JB);"
                    , " a p-value for a test of first-order autocorrelation (pval ac1); and"
                    , " a p-value for a test of first-order autocorrelation in squared values (pval ac1 (squared))."
                      )
  
  chart_blurb<-paste0("Each chart is a quantile-quantile plot (QQ-plot)."
                    ,"  In each chart, the y-axis are the ordered residuals (actual log IOP less predicted log IOP) and the x-axis is the ordered quantiles expected from a N(0,1) distribution."
                    ,"  A relatively straight line is consistent with approximate normality of the residuals.")
  
  list(stats=out_sum,stats_blurb=stats_blurb
       ,chart=chart_out, chart_blurb=chart_blurb
       ,stats_all=list(out_res_sum,out_res_sqr_sum))
}