fcharts<-function(fname,chartvar,chart_dim,panes=NULL,blurb=NULL,include_name=NULL){
  
  # record blurb and file
  if(!is.null(include_name)){fname<-paste0(fname,include_name)}
  #if(!is.null(blurb)) { write(paste0(flatex_fix(blurb),"\\endinput"),paste0(fname,".blurb")) }
  if(!is.null(blurb)) { 
    if(!is.null(include_name)){extra<-paste0(" \\emph{\\tiny ",include_name, "}\\endinput")} else {extra<-"\\endinput"}
    write(paste0(flatex_fix(blurb),flatex_fix(extra)),paste0(fname,".blurb"))
  }
  
  #do charts
  pdf(paste0(fname,".pdf"),width=chart_dim[1],height=chart_dim[2])
    if(!is.null(panes)){par(mfrow=panes)}
    show(chartvar)
  dev.off()
  
  tiff(paste0(fname,".tiff"),width=chart_dim[1],height=chart_dim[2], units = 'in', res = 300, compression = "lzw")
    if(!is.null(panes)){par(mfrow=panes)}
    show(chartvar)
  dev.off()
  
  
  
  # png(paste0(fname,".png"),width=chart_dim[1]*96*.75,height=chart_dim[2]*96*.75)
  #   show(chartvar)
  # dev.off()
  
  # bmp(paste0(fname,"_2.bmp"),width=chart_dim[1],height=chart_dim[2], units = 'in', res = 125)
  #   show(chartvar)
  # dev.off()
}