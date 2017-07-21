before.start.fitting<-function(text.ouput){
options(warn=-1)

### give warning below
###
cat("\n The following steps may go wrong. If so, please check\n")
cat(" your data, your model, initial values and/or weightings.\n\n")
readline(" Press Enter to continue...");cat("\n\n")
cat(" Please wait...\n\n")
###
### dev.new()                ### since v1.3.7
### par(mfrow=c(2,2),las=1)  ### since v1.3.7
###
### log to outputs.txt here
###
zz <- file(text.ouput, open="wt")
sink(zz,split=TRUE)   ### use sink(zz.split=TURE) will output to the txt file, as well as the screen at the same time. YJ
description_version()
cat("\n\n")
sink()  ### turn off temporarily to avoid logging too many warnings... -YJ
return(zz)
}