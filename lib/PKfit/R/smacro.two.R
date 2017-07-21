### Two exponential term
### a one-compartment PK model with the 1st-ordered absorption and 
### the 1st-ordered elmination rate constant  --YJ

smacro.two <-function(Subject=NULL,  # N Subj's 
                      PKtime=NULL,   # times for sampling
                      A=NULL,
                      alpha=NULL,
                      ### B=NULL,
                      beta=NULL,
                      MD=FALSE)
{
   options(warn=-1)
   xaxis <- "time after dosing"
   yaxis <- "simulated drug plasma conc."
   
   sim.outputs_to_txt<-sim.outputs_to_txt;sim.plots_to_pdf<-sim.plots_to_pdf
      
cat("\n First enter all paramaters used for simulation profile.\n")
readline(" Press Enter to continue...");cat("\n\n")
if(file.exists("smacro_two.csv")){
   par<-read.csv(file="smacro_two.csv",row.names=NULL,header=TRUE)
   par<-edit(par)}
else{ 
   par<-data.frame(Parameter=c("Total_subject#","A","alpha","beta"),Initial=c(24,11.6,0.12,0.51))
   par<-edit(par)
   }
   par<-check.para(par)
   write.csv(par,file="smacro_two.csv",row.names=FALSE) 
   cat("\n")       
   print(par, row.names=F)
   
   cat("\n")
   Subject<-par[1,2]
   par1<-par[2,2]
   par2<-par[3,2]
   par3<-par[4,2]

   readline("\n Next enter or load time points. Press Enter to continue.\n\n")
   if(file.exists("sim_times.csv")){
      PKtime<-read.csv(file="sim_times.csv",row.names=NULL,header=TRUE)
      PKtime<-PKtime$time  ### convert to numeric
      PKtime<-edit(PKtime)
      PKtime<-as.data.frame(PKtime);colnames(PKtime)<-"time"}
   else{
      PKtime<-c(0,.1,.2,.3,.4,.6,.8,1,2,4,6,8,12,14,16,18,24,48,72)
      PKtime<-edit(PKtime)
      PKtime<-as.data.frame(PKtime);colnames(PKtime)<-"time"}
   
   write.csv(PKtime,file="sim_times.csv",row.names=FALSE)
   cat("\n")
   print(PKtime, row.names=F);cat("\n\n")
    
    file.menu <- c("Simulation with Error",
                   "Monte-Carlo Simulation")
    pick <- menu(file.menu, title = "<< Simulation Types >>") 
###
### dev.new()
### par(mfrow=c(2,1),las=1)
### pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
### pdf(sim.plots_to_pdf,paper="a4")
###
###
### log to outputs.txt here
###
zz <- file(sim.outputs_to_txt, open="wt")
sink(zz,split=TRUE)   ### use sink(zz.split=TURE) will output to the txt file, as well as the screen at the same time. YJ
description_version()
sink()
###     
    if (pick ==1){
       choose<-para_err("sim")
       pick<-choose$pick
       type<-choose$type

      sink(zz,split=TRUE)

      if (pick ==1){
          ### dev.new() ### no dev.new() under RStudio! since v1.3.7
          ### par(mfrow=c(2,1),las=1)
          pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
          set.plot.windows()
          Subject<-1          ### does not make any sense to simulate more than 1 subject with this option.
          PKindex<-vector(Subject,mode="list")
          for(i in 1:Subject)  {
            cat("\n\n     << Subject:- #",i,">>" )
            A    <-par1
            alpha<-par2  
            beta <-par3
            PKindex[[i]]<-smacro.two.out(PKtime,A,alpha,beta,defun,par1,par2,par3,i,type,xaxis,yaxis,MD)
            save.plots.as.pdf(sim.plots_to_pdf,pdf_activate)
            pdf_activate=TRUE
            }  
      PKindex<- as.data.frame(do.call("rbind",PKindex))
      rownames(PKindex) <- seq(nrow(PKindex)) 
      ### savefile(PKindex)  
      }  
      else {
         par.err<-data.frame(Parameter=c("A","alpha","beta"),error_factor=c(.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         
         ### dev.new() ### no dev.new() under RStudio! since v1.3.7
         ### par(mfrow=c(2,1),las=1)
         pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
         set.plot.windows()
         
         PKindex<-vector(Subject,mode="list")
         pick<- pick-1  # reset pick = 1, 2, 3 or 4 ('0' is zero error & excluded)
         
         for( i in 1:Subject)  { 

             A     <- err_types(pick,par1,factor1)
             alpha <- err_types(pick,par2,factor2)
             beta  <- err_types(pick,par3,factor3)
                      
            cat("\n\n       << Subject:- #",i,">>" )    ### used to implement. -YJ
            PKindex[[i]]<-smacro.two.out(PKtime,A,alpha,beta,defun,par1,par2,par3,i,type,xaxis,yaxis,MD)
            save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
            }   
         PKindex<- as.data.frame(do.call("rbind",PKindex))
         rownames(PKindex) <- seq(nrow(PKindex)) 
         ### savefile(PKindex)     
        } 
      }
      else if (pick ==2){ ### starting monte-carlo sim here
         choose<-para_err("mc")
         pick<-choose$pick
         type<-choose$type
      
         par.err<-data.frame(Parameter=c("A","alpha","beta"),error_factor=c(.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         
      cat("\n\nHow many iterations to run for each subject?\n")
      re<-scan(nlines=1,quiet=TRUE)
      sink(zz,split=TRUE)
      set.plot.windows()
      
      cat("\n\n")
      cat("**********************************\n")
      cat(" Summary Table (Monte-Carlo simulation runs)\n")
      cat(" Model: two-exponential term model \n") 
      cat(" Subject #:", Subject,"            \n")
      cat(" Error Type:", type,"              \n")
      cat(" Simulation #:", re,"              \n\n")
      sim<-matrix(c(par1,par2,par3,factor1,factor2,factor3),3,2)
      dimnames(sim)<-list(c("A","alpha","beta"),c("Input Values","Error factor"))
      print(par, row.names=F);cat("\n\n")  ### since v1.3.3
      print(sim, row.names=F)   
      cat("**********************************")

      ### dev.new() ### no dev.new() under RStudio! since v1.3.7
      ### par(mfrow=c(2,1),las=1)
      pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning

      PKindex<-vector(Subject,mode="list")
      for( i in 1:Subject)  {
        cat("\n\n   << Subject:- #",i," >>\n\n" )    ### used to implement. -YJ
        C1.lsoda<-list()
           for (j in 1:re){
           
             A     <- err_types(pick,par1,factor1)
             alpha <- err_types(pick,par2,factor2)
             beta  <- err_types(pick,par3,factor3)
                        
              time1<-PKtime$time
              defun<- A*(exp(-alpha*time1)-exp(-beta*time1))
              XX<-data.frame(time1,defun) 
              C1.lsoda[[j]]<-data.frame(XX$time1,XX$defun) 
              colnames(C1.lsoda[[j]])<-list("time","concentration")
           }
            PKindex[[i]]<-montecarlo(C1.lsoda,time1,i,re,xaxis,yaxis)
            save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
            montecarlo2(PKindex[[i]],i,re,xaxis,yaxis)        ### since v1.3.3
            save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
         }
    PKindex<-as.data.frame(do.call("rbind",PKindex))
    rownames(PKindex)<-seq(nrow(PKindex)) 
    ### savefile(PKindex)  
   } 
   savefile(PKindex)
   after.fitting(zz,type="sim")  ### since v1.3.7
   ### PK.sim()
   run(first.run=FALSE)
}   
