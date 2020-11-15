cat("\014")  # clean console window
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv) # remove all variables and start again :)

# load all function files in function folder (files begining with "f" only)
function_files<-paste0("source('functions//",list.files(path="functions//",pattern="^f.*.R$"),"', encoding='utf-8')")
for(i in function_files){ eval(parse(text=i)) }
colour_p<-c("#E768AD") # pink for some charts!

# read in raw file, formated with columns set by a tab
temp<-read.csv(file="DATA_academic_burnout.csv",check.names = FALSE,stringsAsFactors =TRUE , na.strings = c("NA"," ",""))  # upload from clean file tab delimited (\t)
View(temp)
str(temp)


############################################################################################
##  challenges ...
############################################################################################
