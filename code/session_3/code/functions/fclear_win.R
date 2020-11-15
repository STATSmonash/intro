clear_win<-function(){ x<-dev.list() ; if(length(x)>=1) {for (i in 1:length(x)){ dev.off(x[i]) }} }
