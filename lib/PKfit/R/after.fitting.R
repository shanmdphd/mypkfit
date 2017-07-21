after.fitting<-function(closedfile,type){

### since v1.3.7
if(type=="fit"){fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf} else
  {sim.outputs_to_txt<-sim.outputs_to_txt;sim.plots_to_pdf<-sim.plots_to_pdf;
   sim.data_to_csv<-sim.data_to_csv;sim.data_to_RData<-sim.data_to_RData}
################

sink()                  ### reset sink()
close(closedfile)       ### close outputs.txt
if(type=="fit")         ### since v1.3.7
 {cat("\n\n Two outputs,",fit.outputs_to_txt,"&\n"," ",fit.plots_to_pdf,",\n have been generated at",getwd(),".\n")}
else
 {cat("\n\n Two outputs,",sim.outputs_to_txt,"&\n"," ",sim.plots_to_pdf,",\n plus two files of simulated dataset\n have been generated at",getwd(),".\n")}

dev.off(which=dev.list()["pdf"]) ### finally close the pdf file now.
}