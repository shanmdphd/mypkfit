OutputFilez<-function(){

##
## these lines are to avoid the error messages of "not visible binding ..." with codetool
##
   fit.outputs_to_txt<- NULL
   fit.plots_to_pdf  <- NULL
   sim.outputs_to_txt<- NULL
   sim.plots_to_pdf  <- NULL
   sim.data_to_csv   <- NULL
   sim.data_to_RData <- NULL
   fit.data_to_csv   <- NULL
   fit.data_to_RData <- NULL
   sim.outputfile    <- "pkfit_sim"
   fit.outputfile    <- "pkfit_fit"
   xFile_ext<-NULL
   xFile_ext<-paste(sample(1001001:9786999,1,replace=F),"_",sep="") ## as random run batch#
   
   fit.outputs_to_txt <<- paste(xFile_ext,fit.outputfile,"_outputs.txt",sep="")
   fit.plots_to_pdf   <<- paste(xFile_ext,fit.outputfile,"_plots.pdf",sep="")
   sim.outputs_to_txt <<- paste(xFile_ext,sim.outputfile,"_outputs.txt",sep="")
   sim.plots_to_pdf   <<- paste(xFile_ext,sim.outputfile,"_plots.pdf",sep="")
   sim.data_to_csv    <<- paste(xFile_ext,sim.outputfile,"_data.csv",sep="")
   sim.data_to_RData  <<- paste(xFile_ext,sim.outputfile,"_data.RData",sep="")
   fit.data_to_csv    <<- paste(xFile_ext,"PKfit_input_data",".csv",sep="")
   fit.data_to_RData  <<- paste(xFile_ext,"PKfit_input_data",".RData",sep="")
}
