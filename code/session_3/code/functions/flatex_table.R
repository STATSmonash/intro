flatex_table<-function(x,dp=NULL,firstcoltype="l",coltype="r",dpextra=NULL,nohead=FALSE,rowspaces=NULL,rowsbold=NULL
                 ,table.type="tabular",table.width=NULL,man.adj=NULL,addrownames=TRUE,NAswap=NA,dfconvert=TRUE,dpextra_col=NULL) {

  #makes a nice table of output for latex
  
  # table type pre-set
  x.raw<-x
  if(dfconvert&!is.data.frame(x)){
    x<-data.frame(x,check.names = FALSE,stringsAsFactors = FALSE) # added 23/05/2020 CHECK
  }
  
  #format (if not pre-formatted will expect numeric to round)
  if(!is.null(dp)){
    for(i in 1:ncol(x)){
      if(is.numeric(x[,i])){
        #x<-apply(x,c(1,2),function(i){if(is.numeric(i)){format(round(i,dp),nsmall=dp,scientific=FALSE)} else {i} })
        x[,i]<-format(round(x[,i],dp),nsmall=dp,scientific=FALSE)
      }

    }
  }
  
  #x<-apply(x,c(1,2),function(i){if(is.numeric(i)){format(round(i,dp),nsmall=dp,scientific=FALSE)} else {i} })
  
  if(!is.null(dpextra)){ for(i in 1:nrow(dpextra)) {
    
    x[dpextra[i,1],]<-format(round(as.numeric(x[dpextra[i,1],]),dpextra[i,2]),nsmall=dpextra[i,2])
    
  }}
  
  if(!is.null(dpextra_col)){ for(i in 1:nrow(dpextra_col)) {
    
    x[,dpextra_col[i,1]]<-format(round(as.numeric(x.raw[,dpextra_col[i,1]]),digits=dpextra_col[i,2]),nsmall=dpextra_col[i,2])
    
  }}
  
  
  
  #adjustments (expects dataframe [row to adjust, check level , new value])
  if(!is.null(man.adj)){ for(i in 1:nrow(man.adj)) {
    
    x[man.adj[i,1],x.raw[man.adj[i,1],]==man.adj[i,2] ]<-man.adj[i,3]
    
    
  }}
  
  # adjust for NAs to blank?
  if(!is.na(NAswap)){
    #can only be done for non-numeric
    x<-apply(x,c(1,2),as.character)
    x[is.na(x.raw)]<-NAswap
  }
  
  #add in col breaks
  if(ncol(x)>1){
    x[,1:(ncol(x)-1)]<-apply(as.matrix(x[,1:(ncol(x)-1)]),c(1,2), function(i){paste0(i," &")})
  }
  x[,ncol(x)]<-paste(x[,ncol(x)],"\\\\")
  

  
  # add header (main data defaults to "r", otherwise specify and match in latex)
  if(nchar(coltype)==1){cols<-rep(coltype,ncol(x))} else {cols<-coltype}
  col.first<-NULL
  if(!is.null(rownames(x))&addrownames){cols<-c(firstcoltype,cols) ; col.first<-" & "}
  #[insert] option for specifying col formats...
  cols.align<-paste0(c("{",cols,"}"),collapse ="")
  
  col.names<-colnames(x)
  
  if(!is.null(colnames(x))){ #col.names<-paste(col.first,col.names) 
    col.names<-paste0(col.names,collapse=" & ") 
    col.names<-paste(col.first,col.names,sep="")
    col.names<-paste( col.names ," \\\\ \\midrule",sep="") }
  
  tab.head<-paste0("\\begin{",table.type,"}",table.width,cols.align, " \\toprule ",col.names ,collapse="")
  
  # add footer
  tab.foot<-paste0("\\bottomrule \\end{",table.type,"}")
  
  #make into vector
  y<-matrix(apply(x, 1, paste, collapse=" "),ncol=1)
  # add in row names
  if(!is.null(rownames(x)) & addrownames) {y<-as.matrix(paste(rownames(x)," &",y))}
  #combine
  y<-rbind(tab.head,y,tab.foot, deparse.level = 0)
  
  if(nohead){y<-y[-c(1,nrow(y)),]}
  
  #conversion for escape characters (eg. in headings)
  # fix for special characters
  fix<-c("_","%","#")
  #fix<-c("%","#")
  for(i in 1:length(fix)){ y<- gsub(fix[i],paste0("\\\\",fix[i]),y,perl = TRUE)}
  
  fix2<-cbind(c("££"),c("_"))
  for(i in 1:nrow(fix2)){ y<- gsub(fix2[i,1],paste0(fix2[i,2]),y,perl = TRUE)}
  
  ##INSERT bold rows ...
  if(!is.null(rowsbold)){
    y<-as.matrix(y)
    y[rowsbold,]<-paste(" \\bfseries" , gsub("&","& \\\\bfseries ",y[rowsbold,],perl = TRUE) )
  }
  
  ##INSERT row spaces
  if(!is.null(rowspaces)){
    y.temp<-as.vector(y)
    rowspaces<-c(sort(rowspaces))
    spacer<-paste(col.first,paste(rep(" & ",ncol(x)-1),collapse=""),"\\\\")
    
    y.temp<-c(y,rep(spacer,length(rowspaces)))
    y.temp<-y.temp[sort.list(c(1:length(y),rowspaces))]
    
    y<-as.matrix(y.temp)
  }
  
  y
}