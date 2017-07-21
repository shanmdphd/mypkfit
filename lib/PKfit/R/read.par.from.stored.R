###
### this function is to read (load) previous parameter set from fitting or simulation run
###
read.par.from.stored<- function(MD,fitpar,simpar){
if(MD){
if(file.exists(fitpar)){
        par.init<-read.csv(file=fitpar,row.names=NULL,header=TRUE)
        cat("\n loading parameter set from previous fitting profile...\n ")
        par.init<-edit(par.init)}
   else{
        if(file.exists(simpar)){                       ### read parameter set from simulation
        par.init<-read.csv(file=simpar,row.names=NULL,header=TRUE)
        cat("\n loading parameter set from previous simulation profile...\n ")
        par.init<-par.init[-1,]     ### delete the first row (i.e., Subject# to simulate)
        write.csv(par.init,file=fitpar,row.names=F)   ### save to reset the row numbering
        par.init<-read.csv(file=fitpar,row.names=NULL,header=TRUE)
        par.init<-edit(par.init)    ### edit par.init now
        }
     }
   }
else{ ### single-dose
if(file.exists(fitpar)){
     par.init<-read.csv(file=fitpar,row.names=NULL,header=TRUE)
     cat("\n loading parameter set from previous fitting profile...\n ")
     par.init<-edit(par.init)}
else{
     if(file.exists(simpar)){                       ### read parameter set from simulation
     par.init<-read.csv(file=simpar,row.names=NULL,header=TRUE)
     cat("\n loading parameter set from previous simulation profile...\n ")
     par.init<-par.init[-1,]     ### delete the first row (i.e., Subject# to simulate)
     write.csv(par.init,file=fitpar,row.names=F)   ### save to reset the row numbering
     par.init<-read.csv(file=fitpar,row.names=NULL,header=TRUE)
     par.init<-edit(par.init)    ### edit par.init now
     }
  }
}
  return(par.init)
}
