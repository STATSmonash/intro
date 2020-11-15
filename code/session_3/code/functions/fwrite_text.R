############################
#### LATEX
############################
fwrite_text<-function(filename, text){
  fileConn<-file(filename)
  writeLines(text, fileConn)
  close(fileConn)
}


