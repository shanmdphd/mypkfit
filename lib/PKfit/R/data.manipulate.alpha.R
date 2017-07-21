#Data manipulation
data.manipulate.alpha <- function()
{
  file.menu <- c("Load Data Files (.CSV)", 
                 "Load Data Files (.RData)", 
                 "Input data from keyboard", 
                 "Go Back One Upper Level")  
  
  pick <- menu(file.menu, title = "<< Data load/input/edit >>")
  
  if (pick == 1){
     cat("\n")
     cnames<-c("Subject", "time", "conc")
     ### PKindex<-read.csv(file.choose(),header=TRUE,sep=",",row.names=NULL,col.names=cnames)
     PKindex<-fread(file.choose(),header=TRUE,col.names=cnames,
                    showProgress=getOption("datatable.showProgress"))   # default: TRUE
     PKindex<-edit(PKindex)
     cat("\n\n")
     print(PKindex, row.names=F)
     cat("\n\n")
     return(macro(PKindex))}
     
  if (pick == 2){
     PKindex<-readRDS(file.choose())
     PKindex<-edit(PKindex)
     colnames(PKindex)<-list("Subject", "time", "conc")
     cat("\n\n")
     print(PKindex, row.names=F)
     cat("\n\n")
     return(macro(PKindex))}   
  
  if (pick == 3){
     PKindex<-data.frame(Subject=c(1),time=c(0),conc=c(0))
     PKindex<-edit(PKindex)
     cat("\n\n") 
     print(PKindex, row.names=F)
     pkfit.to.save.dataset(PKindex)
     cat("\n\n")
     return(macro(PKindex))}
  
  if (pick == 4){cat("\n\n");PK.fit(PKindex)}
}
