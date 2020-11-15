flatex_write<-function(fname,txt=NULL,blurb=NULL,csv=NULL,include_name=NULL,csv_rownames=FALSE){
  
  if(!is.null(include_name)){fname<-paste0(fname,include_name)}
  
  if(!is.null(txt))   { write(txt,paste0(fname,".txt")) } # use this for flatex_table output
  
  if(!is.null(csv))   { write.csv(csv,file=paste0(fname,".csv"), row.names = csv_rownames) 
    write.table(c("",blurb),file=paste0(fname,".csv") , append = TRUE,  sep=',', row.names=F, col.names=F )} # use this for flatex_table output
  
  
  #if(!is.null(blurb)) { write(paste0(ian_latex_fix(blurb),"\\endinput"),paste0(fname,".blurb")) }
  
  if(!is.null(blurb)) { 
    if(!is.null(include_name)){extra<-paste0(" \\emph{\\tiny ",include_name, "}\\endinput")} else {extra<-"\\endinput"}
    write(paste0(flatex_fix(blurb),flatex_fix(extra)),paste0(fname,".blurb"))
  }
  
  
}

flatex_fix<-function(x){
  fix<-c("_","%","#","&")
  for(i in 1:length(fix)){ x<- gsub(fix[i],paste0("\\\\",fix[i]),x,perl = TRUE)}
  
  fix2<-cbind(c('££'),c("_"))
  for(i in 1:nrow(fix2)){ x<- gsub(fix2[i,1],paste0(fix2[i,2]),x,perl = TRUE)}
  
  x
  
}

## helper function to get associated blurb for tables and figures ...
## the "\endinput" prevents latex from adding a space after using "\input"