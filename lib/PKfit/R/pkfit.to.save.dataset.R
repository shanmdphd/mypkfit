###
### use this function to save user's keyboard input dataset; called by 
### data.mmanipulate() and data.manipulate.alpha()
##
pkfit.to.save.dataset<-function(PKindex){

fit.data_to_csv   <- fit.data_to_csv
fit.data_to_RData <- fit.data_to_RData

write.csv(PKindex,file=fit.data_to_csv,row.names=FALSE)
saveRDS(PKindex,file=fit.data_to_RData)
cat("\n\n");cat(paste(" Your input data has been saved as\n"," ",fit.data_to_csv," &\n",
    " ",fit.data_to_RData,"\n in the working directory of\n"," ",getwd(),sep=""))
cat("\n *** You can rename these data files\n later if you like.");cat("\n\n")
readline(" Press Enter to continue...")
}