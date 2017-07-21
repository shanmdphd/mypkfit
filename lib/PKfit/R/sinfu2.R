### Two compartment PK model iv infusion single dose
sinfu2<- function(Subject=NULL,  # N Subj's 
                  PKtime=NULL,   # times for sampling
                  Dose=NULL,     # single dose
                  Tinf=NULL,
                  Vd=NULL,
                  kel=NULL, 
                  k12=NULL,
                  k21=NULL,
                  nDose=NULL,    # for multiple-dose
                  Tau=NULL,
                  MD=FALSE)
{
   options(warn=-1)
   xaxis <- "time after dosing"
   yaxis <- "simulated drug plasma conc."
   
   sim.outputs_to_txt<-sim.outputs_to_txt;sim.plots_to_pdf<-sim.plots_to_pdf
      
cat("\n First enter all paramaters used for simulation profile.\n")
readline(" Press Enter to continue...");cat("\n\n")
if(MD) {
if(file.exists("sinfu2_md.csv")){
   par<-read.csv(file="sinfu2_md.csv",row.names=NULL,header=TRUE)
   par<-edit(par)}
else{  
   par<-data.frame(Parameter=c("Total_subject#","Dose","Tau","#Dose","Tinf","kel","k12","k21","Vd"),
                     Initial=c(24,300,8,10,1.5,0.26,0.13,0.31,11.7))
   par<-edit(par)
   }                                                   
   par<-check.para(par)
   write.csv(par,file="sinfu2_md.csv",row.names=FALSE) 
   cat("\n")       
   print(par, row.names=F)
   
   cat("\n")
   Subject<-par[1,2]
   Dose <-par[2,2]
   Tau  <-par[3,2]
   nDose<-par[4,2]
   Tinf<-par[5,2]
   par1<-par[6,2]
   par2<-par[7,2]
   par3<-par[8,2]
   par4<-par[9,2]

### no PKtime require for multiple-dose simulation
PKtime<-data.frame(time=seq(0,Tau*nDose-0.1,0.1))  ### to see more output details, smaller time steps. -YJ
  
    defun<- function(time, y, parms) { 
       if(time%%Tau<=Tinf)
           dCp1dt<- (Dose/Tinf)/parms["Vd"]+parms["k21"]*y[2]-parms["kel"]*y[1]-parms["k12"]*y[1]
       else 
           dCp1dt<- -parms["kel"]*y[1]-parms["k12"]*y[1]+parms["k21"]*y[2] 
           dCp2dt<- parms["k12"]*y[1]-parms["k21"]*y[2]
       list(c(dCp1dt,dCp2dt)) 
   }
}
else{       #### single-dosed ####
if(file.exists("sinfu2.csv")){
   par<-read.csv(file="sinfu2.csv",row.names=NULL,header=TRUE)
   par<-edit(par)}
else{  
   par<-data.frame(Parameter=c("Total_subject#","Dose","Tinf","kel","k12","k21","Vd"),Initial=c(24,300,1.5,0.26,0.13,0.31,11.7))
   par<-edit(par)
   }                                                     
   par<-check.para(par)
   write.csv(par,file="sinfu2.csv",row.names=FALSE) 
   cat("\n")       
   print(par, row.names=F)
   
   cat("\n")
   Subject<-par[1,2]
   Dose<-par[2,2]
   Tinf<-par[3,2]
   par1<-par[4,2]
   par2<-par[5,2]
   par3<-par[6,2]
   par4<-par[7,2]

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
  
    defun<- function(time, y, parms) { 
       if(time<=Tinf) {
           dCp1dt<- (Dose/Tinf)/parms["Vd"]+parms["k21"]*y[2]-parms["kel"]*y[1]-parms["k12"]*y[1]
           dCp2dt<- parms["k12"]*y[1]-parms["k21"]*y[2]
         }
       else {       
           dCp1dt <- -parms["kel"]*y[1]-parms["k12"]*y[1]+parms["k21"]*y[2] 
           dCp2dt <- parms["k12"]*y[1]-parms["k21"]*y[2]
         }
       list(c(dCp1dt,dCp2dt)) 
   }
}
     
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
         for( i in 1:Subject)  {
           cat("\n\n     << Subject:- #",i,">>" )
           cat("\n\n")                                                               
           cat("*******************************************************\n")        
           cat(" Summary Table                                          \n")   
           if(MD)
           cat(" Model: 2-compartment, iv infusion, multiple-dose model\n\n")
           else    
           cat(" Model: 2-compartment, iv infusion, single-dose model \n\n")
           cat("      Subject #:", Subject,"                       \n")
           cat("           Dose:", Dose,"                          \n") 
           if(MD) {
           cat("Dosing Interval:", Tau,"                           \n")
           cat("      # of Dose:", nDose,"                         \n")}
           cat("  Infusion time:", Tinf,"                          \n")
           cat("     Error Type:", type,"\n\n")     
           sim<-matrix(c(kel,k12,k21,Vd,par1,par2,par3,par4),4,2)
           dimnames(sim)<-list(c("kel","k12","k21","Vd"),c("Simulated Values","Input Values"))
           print(sim, row.names=F)                                                               
           cat("*******************************************************\n\n")
           
           kel<-par1
           k12<-par2
           k21<-par3
           Vd <-par4   
           ### PKindex[[i]]<-sinfu2.out(PKtime,kel,k12,k21,Vd,defun,par1,par2,par3,par4,Dose,i,type,MD)
           time<-PKtime$time
           parms<-c(kel=kel,k12=k12,k21=k21,Vd=Vd)
           if(MD){
             dosing.time<-seq(0,Tau*nDose,Tau)
             yini<-c(dCp1dt=0,dCp2dt=0)
             C1.lsoda<-data.frame(lsode(yini,c(0,time),defun,parms,rtol=1e-08,atol=1e-08))
           }
           else{
             C1.lsoda<-data.frame(lsoda(c(0,0),c(0,time),defun,parms,rtol=1e-08,atol=1e-08))}
           
           good<-ifelse(C1.lsoda[2:(length(time)+1),2]<=0,
                        0,
                        C1.lsoda[2:(length(time)+1),2])
           good<-add.err(good)   ### add random errors to integrated Cp here
           PKindex.this.subj<-data.frame(i,C1.lsoda[2:(length(time)+1),1],good)
           colnames(PKindex.this.subj)<-list("Subject","time","conc")
           print(head(PKindex.this.subj,10), row.names=F);cat("... (the rest of data omitted)\n")   ### since v1.3.7
           x<-C1.lsoda[2:(length(time)+1),1]
           y<-good
           plotting.sim(i,x,y,xaxis,yaxis,MD);PKindex[[i]]<-PKindex.this.subj   ### dump this subj's data to subj i.
           save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
         }   
         PKindex<- as.data.frame(do.call("rbind",PKindex))
         rownames(PKindex) <- seq(nrow(PKindex)) 
         ### savefile(PKindex)    
       }   
       else {
         par.err<-data.frame(Parameter=c("kel","k12","k21","Vd"),error_factor=c(.15,.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         factor4<-par.err[4,2]
         
         ### dev.new() ### no dev.new() under RStudio! since v1.3.7
         ### par(mfrow=c(2,1),las=1)
         pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
         set.plot.windows()
         
         PKindex<-vector(Subject,mode="list")
         pick<- pick-1  # reset pick = 1, 2, 3 or 4 ('0' is zero error & excluded)
         
         for( i in 1:Subject)  {
         
               kel <- err_types(pick,par1,factor1)
               k12 <- err_types(pick,par2,factor2)
               k21 <- err_types(pick,par3,factor3)
               Vd  <- err_types(pick,par4,factor4)

               cat("\n\n     << Subject:- #",i,">>" )
               cat("\n\n")                                                               
               cat("*******************************************************\n")        
               cat(" Summary Table                                          \n")   
               if(MD)
               cat(" Model: 2-compartment, iv infusion, multiple-dose model\n\n")
               else    
               cat(" Model: 2-compartment, iv infusion, single-dose model \n\n")
               cat("      Subject #:", Subject,"                       \n")
               cat("           Dose:", Dose,"                          \n") 
               if(MD) {
               cat("Dosing Interval:", Tau,"                           \n")
               cat("      # of Dose:", nDose,"                         \n")}
               cat("  Infusion time:", Tinf,"                          \n")
               cat("     Error Type:", type,"\n\n")     
               sim<-matrix(c(kel,k12,k21,Vd,par1,par2,par3,par4),4,2)
               dimnames(sim)<-list(c("kel","k12","k21","Vd"),c("Simulated Values","Input Values"))
               print(sim, row.names=F)                                                               
               cat("*******************************************************\n\n")
               
               ### PKindex[[i]]<-sinfu2.out(PKtime,kel,k12,k21,Vd,defun,par1,par2,par3,par4,Dose,i,type,MD)
               time<-PKtime$time
               parms<-c(kel=kel,k12=k12,k21=k21,Vd=Vd)
               if(MD){
                 dosing.time<-seq(0,Tau*nDose,Tau)
                 yini<-c(dCp1dt=0,dCp2dt=0)
                 C1.lsoda<-data.frame(lsode(yini,c(0,time),defun,parms,rtol=1e-08,atol=1e-08))
               }
               else{
                 C1.lsoda<-data.frame(lsoda(c(0,0),c(0,time),defun,parms,rtol=1e-08,atol=1e-08))}
               
               good<-ifelse(C1.lsoda[2:(length(time)+1),2]<=0,
                            0,
                            C1.lsoda[2:(length(time)+1),2])
               good<-add.err(good)   ### add random errors to integrated Cp here
               PKindex.this.subj<-data.frame(i,C1.lsoda[2:(length(time)+1),1],good)
               colnames(PKindex.this.subj)<-list("Subject","time","conc")
               print(head(PKindex.this.subj,10), row.names=F);cat("... (the rest of data omitted)\n")   ### since v1.3.7
               x<-C1.lsoda[2:(length(time)+1),1]
               y<-good
               plotting.sim(i,x,y,xaxis,yaxis,MD);PKindex[[i]]<-PKindex.this.subj
               save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
     } 
     PKindex<- as.data.frame(do.call("rbind",PKindex))
     rownames(PKindex) <- seq(nrow(PKindex)) 
     ### savefile(PKindex)          
    }
  }
  else if (pick ==2){ 
     choose<-para_err("mc")
     pick<-choose$pick
     type<-choose$type
     
         par.err<-data.frame(Parameter=c("kel","k12","k21","Vd"),error_factor=c(.15,.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         factor4<-par.err[4,2]
         
     cat("\n\nHow many iterations to run for each subject?\n")
     re<-scan(nlines=1,quiet=TRUE)
     sink(zz,split=TRUE)
     set.plot.windows()
     
     cat("\n\n")
     cat("****************************************************\n")
     cat("Summary Table - Monte-Carlo simulation runs         \n")
     if(MD)
     cat("Model: 2-compartment, iv infusion, multiple-dose model \n\n") 
     else
     cat("Model: 2-compartment, iv infusion, single-dose model   \n\n") 
     cat("      Subject #:", Subject,"                      \n")
     if(MD) {
     cat("Dosing Interval:", Tau,"                          \n")
     cat("      # of Dose:", nDose,"                        \n")}
     cat("  Infusion time:", Tinf,"                          \n")
     cat("   Simulation #:", re,"                            \n\n")
     cat("     Error Type:", type,"                          \n")
     sim<-matrix(c(par1,par2,par3,par4,factor1,factor2,factor3,factor4),4,2)
     dimnames(sim)<-list(c("kel","k12","k21","Vd"),c("Input Values","Error factor"))
     print(par, row.names=F);cat("\n\n")  ### since v1.3.3
     print(sim, row.names=F)   
     cat("*******************************************************")

     ### dev.new() ### no dev.new() under RStudio! since v1.3.7
     ### par(mfrow=c(2,1),las=1)
     pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
     
     PKindex<-vector(Subject,mode="list")
     for( i in 1:Subject)  {
     cat("\n\n     << Subject:- #",i,">>\n" )
     C1.lsoda<-list()
        for (j in 1:re){
        
               kel <- err_types(pick,par1,factor1)
               k12 <- err_types(pick,par2,factor2)
               k21 <- err_types(pick,par3,factor3)
               Vd  <- err_types(pick,par4,factor4)

               time1<-PKtime$time
               parms<-c(kel=kel,k12=k12,k21=k21,Vd=Vd)  
               if(MD){
                 dosing.time<-seq(0,Tau*nDose,Tau)
                 yini<-c(dCp1dt=0,dCp2dt=0)
                 XX<-data.frame(lsode(yini,c(0,time1),defun,parms,rtol=1e-08,atol=1e-08))
               }
               else{
               XX<-data.frame(lsoda(c(0,0),c(0,time1),defun,parms,rtol=1e-08,atol=1e-08))}
               C1.lsoda[[j]]<-data.frame(XX[2:(length(time1)+1),1],XX[2:(length(time1)+1),2])
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
   ### if(MD) stwo.MD.all() else stwo.SD.all()
   run(first.run=FALSE)
}   
