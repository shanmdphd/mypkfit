###Simulation
###One compartment PK model extravascualr single dose first-order absorption 
###optional Michaelis-Menten Elimination
###optional lag time
sfirst.nolag <- function(Subject=NULL,  # N Subj's 
                         PKtime=NULL,   # times for sampling
                         Dose=NULL,     # single dose
                         Tlag_time=NULL,
                         ka=NULL,     
                         Vd=NULL,
                         kel=NULL,      ## If not MM elimination
                         MMe=FALSE,     ## michaelis-menten elimination?
                         Vm=NULL,Km=NULL,
                         Tlag=FALSE,
                         nDose=NULL,    ## for multiple-dose
                         Tau=NULL,
                         MD=FALSE)
{
   options(warn=-1)
   xaxis <- "time after dosing"
   yaxis <- "simulated drug plasma conc."
   
   sim.outputs_to_txt<-sim.outputs_to_txt;sim.plots_to_pdf<-sim.plots_to_pdf
   
cat("\n First enter all paramaters used for simulation profile.\n")
readline(" Press Enter to continue...");cat("\n\n")
if(MD){
if (MMe) {
  if(Tlag){
    if(file.exists("sfirst_lag_mm_md.csv")){
        par<-read.csv(file="sfirst_lag_mm_md.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{       
        par<-data.frame(Parameter=c("Total_subject#","Dose","Tau","#Dose","Tlag","ka","Vm","Km","Vd"),
                          Initial=c(24,300,12,12,0.25,0.36,2.13,4.31,11.7))
        par<-edit(par)
        }                                                       
        par<-check.para(par)
        write.csv(par,file="sfirst_lag_mm_md.csv",row.names=FALSE)
        cat("\n")       
        print(par, row.names=F)
        
        cat("\n")
        Subject<-par[1,2]
        Dose   <-par[2,2]
        Tau    <-par[3,2]
        nDose  <-par[4,2]
        Tlag_time<-par[5,2]
        par1<-par[6,2]
        par2<-par[7,2]
        par3<-par[8,2]
        par4<-par[9,2]
   }
  else{  ### starting 1st-ordered elim. from here 
    if(file.exists("sfirst_nolag_mm_md.csv")){
        par<-read.csv(file="sfirst_nolag_mm_md.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{       
        par<-data.frame(Parameter=c("Total_subject#","Dose","Tau","#Dose","ka","Vm","Km","Vd"),
                          Initial=c(24,300,12,12,0.51,2.13,4.31,11.7))
        par<-edit(par)
        }                                                       
        par<-check.para(par)
        cat("\n")       
        print(par, row.names=F)
        write.csv(par,file="sfirst_nolag_mm_md.csv",row.names=FALSE) 
        cat("\n")
        Subject<-par[1,2]
        Dose   <-par[2,2]
        Tau    <-par[3,2]
        nDose  <-par[4,2]
        par1<-par[5,2]
        par2<-par[6,2]
        par3<-par[7,2]
        par4<-par[8,2]       
       }
   }

else{
  if(Tlag){
    if(file.exists("sfirst_lag_md.csv")){
        par<-read.csv(file="sfirst_lag_md.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{       
        par<-data.frame(Parameter=c("Total_subject#","Dose","Tau","#Dose","Tlag","ka","kel","Vd"),
                          Initial=c(24,300,12,10,0.50,0.36,0.13,11.7))
        par<-edit(par)
        }                                                      
        par<-check.para(par)
        cat("\n")       
        print(par, row.names=F)
        write.csv(par,file="sfirst_lag_md.csv",row.names=FALSE) 
        cat("\n")
        Subject<-par[1,2]
        Dose   <-par[2,2]
        Tau    <-par[3,2]
        nDose  <-par[4,2]
        Tlag_time<-par[5,2]
        par1<-par[6,2]
        par2<-par[7,2]
        par3<-par[8,2]
      }
  else{
    if(file.exists("sfirst_nolag_md.csv")){
        par<-read.csv(file="sfirst_nolag_md.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{    
        par<-data.frame(Parameter=c("Total_subject#","Dose","Tau","#Dose","ka","kel","Vd"),
                          Initial=c(24,300,12,10,0.36,0.13,11.7))
        par<-edit(par)
        }                                                    
        par<-check.para(par)
        cat("\n")       
        print(par, row.names=F)
        write.csv(par,file="sfirst_nolag_md.csv",row.names=FALSE) 
        cat("\n")
        Subject<-par[1,2]
        Dose   <-par[2,2]
        Tau    <-par[3,2]
        nDose  <-par[4,2]
        par1<-par[5,2]
        par2<-par[6,2]
        par3<-par[7,2]
   }
 }
     
### no PKtime require for multiple-dose simulation
PKtime<-data.frame(time=seq(0,Tau*nDose-0.1,0.1))  ### set '0.1' to see more output details.
  
   if (MMe) {
      if (Tlag){
        ## User-supplied function w Michaelis-Mention elimination & w lag time
        defun<- function(time, y, parms) { 
           if(time<=Tau){         ### after the 1st dose
             if(time <= Tlag_time) {
                dy1dt<-0
                dy2dt<-0
             }
             else {
                dy1dt <- -parms["ka"] * y[1]
                dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["Vm"]*y[2]/(parms["Km"]+y[2])
             }
           }
           else{             ### starting 2nd, 3rd, 4th....doses.
            if(time%%Tau<=1 && time%%Tau<=Tlag_time){
               dy1dt<- Dose
               dy2dt<- - parms["Vm"]*y[2]/(parms["Km"]+y[2])
            }
            if(time%%Tau<=1 && time%%Tau>Tlag_time){
               dy1dt<- Dose-parms["ka"] * y[1]
               dy2dt<- parms["ka"] * y[1]/parms["Vd"]- parms["Vm"]*y[2]/(parms["Km"]+y[2])
            }
            if(time%%Tau>1 && time%%Tau<=Tlag_time){
               dy1dt<- 0
               dy2dt<- - parms["Vm"]*y[2]/(parms["Km"]+y[2])
            }
            if(time%%Tau>1 && time%%Tau>Tlag_time){
               dy1dt<- -parms["ka"] * y[1]
               dy2dt<-  parms["ka"] * y[1]/parms["Vd"]- parms["Vm"]*y[2]/(parms["Km"]+y[2])
            }
          }
            list(c(dy1dt,dy2dt)) 
        }
      }
      else{
        ## User-supplied function with MM elimination w/o lag time
        defun<- function(time, y, parms) { 
          if(time<Tau){              ### must consider when time = 0...1; critical!  --YJ
          dy1dt <- -parms["ka"] * y[1]
          dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["Vm"]*y[2]/(parms["Km"]+y[2])
          }
          else{
          if(time%%Tau<=1) dy1dt <- -parms["ka"] * y[1] + Dose
                      else dy1dt <- -parms["ka"] * y[1]
          dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["Vm"]*y[2]/(parms["Km"]+y[2])
          }
          list(c(dy1dt,dy2dt)) 
        } 
      }  
   } 
   else {
      if (Tlag){
        ## User-supplied function w/o MM elimination w lag time
        defun<- function(time, y, parms) {
        if(time<Tau){ 
           if(time<=Tlag_time) {
              dy1dt<-0
              dy2dt<-0
           }
           else {
              dy1dt <- -parms["ka"]*y[1]
              dy2dt <-  parms["ka"]*y[1]/parms["Vd"]-parms["kel"]*y[2]
           }
        }
        else{
            if(time%%Tau<=1 && time%%Tau<=Tlag_time){
               dy1dt<- Dose
               dy2dt<- -parms["kel"]*y[2]
            }
            if(time%%Tau<=1 && time%%Tau>Tlag_time){
               dy1dt<- Dose-parms["ka"] * y[1]
               dy2dt<- parms["ka"] * y[1]/parms["Vd"]-parms["kel"]*y[2]
            }
            if(time%%Tau>1 && time%%Tau<=Tlag_time){
               dy1dt<- 0
               dy2dt<- -parms["kel"]*y[2]
            }
            if(time%%Tau>1 && time%%Tau>Tlag_time){
               dy1dt<- -parms["ka"] * y[1]
               dy2dt<- parms["ka"] * y[1]/parms["Vd"]-parms["kel"]*y[2]
            }
        }
           list(c(dy1dt,dy2dt)) 
         } 
      }
      else{
        ## User-supplied function 1st-ordered abs/elim. w/o lag time
        defun <- function(time, y, parms) { 
          if(time<Tau){              ### must consider when time = 0...1; critical!  --YJ
          dy1dt <- -parms["ka"] * y[1]
          }
          else{
          if(time%%Tau<=1) dy1dt <- -parms["ka"] * y[1] + Dose
                      else dy1dt <- -parms["ka"] * y[1]}
          dy2dt <-  parms["ka"] * y[1]/parms["Vd"] -parms["kel"]*y[2]
          list(c(dy1dt,dy2dt)) 
        } 
      }  
   }
}
else {    ### single-dose sim from here
if (MMe) {
  if(Tlag){
    if(file.exists("sfirst_lag_mm.csv")){
        par<-read.csv(file="sfirst_lag_mm.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{       
        par<-data.frame(Parameter=c("Total_subject#","Dose","Tlag","ka","Vm","Km","Vd"),
                          Initial=c(24,300,0.25,0.36,0.13,0.31,11.7))
        par<-edit(par)                                                       
     }
        par<-check.para(par)
        write.csv(par,file="sfirst_lag_mm.csv",row.names=FALSE)
        cat("\n")       
        print(par, row.names=F)
        
        cat("\n")
        Subject<-par[1,2]
        Dose<-par[2,2]
        Tlag_time<-par[3,2]
        par1<-par[4,2]
        par2<-par[5,2]
        par3<-par[6,2]
        par4<-par[7,2]
   }
  else{
    if(file.exists("sfirst_nolag_mm.csv")){
        par<-read.csv(file="sfirst_nolag_mm.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{       
        par<-data.frame(Parameter=c("Total_subject#","Dose","ka","Vm","Km","Vd"),Initial=c(24,300,0.36,0.13,0.31,11.7))
        par<-edit(par)                                                       
       }
        par<-check.para(par)
        cat("\n")       
        print(par, row.names=F)
        write.csv(par,file="sfirst_nolag_mm.csv",row.names=FALSE) 
        cat("\n")
        Subject<-par[1,2]
        Dose<-par[2,2]
        par1<-par[3,2]
        par2<-par[4,2]
        par3<-par[5,2]
        par4<-par[6,2]       
       }
   }

else{
  if(Tlag){
    if(file.exists("sfirst_lag.csv")){
        par<-read.csv(file="sfirst_lag.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{       
        par<-data.frame(Parameter=c("Total_subject#","Dose","Tlag","ka","kel","Vd"),Initial=c(24,300,0.25,0.36,0.13,11.7))
        par<-edit(par)}                                                       
        par<-check.para(par)
        cat("\n")       
        print(par, row.names=F)
        write.csv(par,file="sfirst_lag.csv",row.names=FALSE) 
        cat("\n")
        Subject<-par[1,2]
        Dose<-par[2,2]
        Tlag_time<-par[3,2]
        par1<-par[4,2]
        par2<-par[5,2]
        par3<-par[6,2]
    }
  else{
    if(file.exists("sfirst_nolag.csv")){
        par<-read.csv(file="sfirst_nolag.csv",row.names=NULL,header=TRUE)
        par<-edit(par)}
    else{    
        par<-data.frame(Parameter=c("Total_subject#","Dose","ka","kel","Vd"),Initial=c(24,300,0.36,0.13,11.7))
        par<-edit(par)}                                                       
        par<-check.para(par)
        cat("\n")       
        print(par, row.names=F)
        write.csv(par,file="sfirst_nolag.csv",row.names=FALSE) 
        cat("\n")
        Subject<-par[1,2]
        Dose<-par[2,2]
        par1<-par[3,2]
        par2<-par[4,2]
        par3<-par[5,2]      
      }
}
     
   readline("\n Next enter or load time points. Press Enter to continue.\n\n")
   if(file.exists("sim_times.csv")){
      PKtime<-read.csv(file="sim_times.csv",row.names=NULL,header=TRUE)
      PKtime<-PKtime$time  ### convert to numeric
      PKtime<-edit(PKtime)
      PKtime<-as.data.frame(PKtime);colnames(PKtime)<-"time"}
   else{
      ### PKtime<-data.frame(time=c(0))
      PKtime<-c(0,.1,.2,.3,.4,.6,.8,1,2,4,6,8,12,14,16,18,24,48,72)
      PKtime<-edit(PKtime)
      PKtime<-as.data.frame(PKtime);colnames(PKtime)<-"time"}
   
   write.csv(PKtime,file="sim_times.csv",row.names=FALSE)
   cat("\n")
   print(PKtime, row.names=F);cat("\n\n")
  
   if (MMe) {
      if (Tlag){
        ## User-supplied function w Michaelis-Mention elimination & w lag time
        defun<- function(time, y, parms) { 
           if(time <= Tlag_time) {
              dy1dt<-0
              dy2dt<-0
           }
           else {
              dy1dt<- -parms["ka"] * y[1]
              dy2dt<-  parms["ka"] * y[1]/parms["Vd"] - parms["Vm"]*y[2]/(parms["Km"]+y[2])
           }
           list(c(dy1dt,dy2dt)) 
         } 
      }
      else{
        ## User-supplied function with MM elimination w/o lag time
        defun<- function(time, y, parms) { 
          dy1dt <- -parms["ka"] * y[1]
          dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["Vm"]*y[2]/(parms["Km"]+y[2])
          list(c(dy1dt,dy2dt)) 
        } 
      }  
   } 
   else {
      if (Tlag){
        ## User-supplied function w/o MM elimination w lag time
        defun<- function(time, y, parms) { 
           if(time<=Tlag_time) {
              dy1dt<-0
              dy2dt<-0
           }
           else {
              dy1dt <- -parms["ka"]*y[1]
              dy2dt <-  parms["ka"]*y[1]/parms["Vd"]-parms["kel"]*y[2]
           }
           list(c(dy1dt,dy2dt)) 
         } 
      }
      else{
        ## User-supplied function w/o MM elimination w/o lag time
        defun <- function(time, y, parms) { 
           dy1dt <- -parms["ka"] * y[1]
           dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["kel"] * y[2]
           list(c(dy1dt,dy2dt)) 
        } 
      }  
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
   
   if (pick ==1){    ### normal simulation starting here
       ### c(pick,type):=para_err("sim")   ### use an unique function here; will return both 'pick' and 'type'; works great.
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
           cat("\n\n")                                                               
           cat("*******************************************************\n")        
           cat(" Summary Table                                       \n\n")   
           if(MD)
           cat(" Model: 1-compartment, extravascular, multiple-dose,  \n")
           else    
           cat(" Model: 1-compartment, extravascular, single-dose,    \n")
           if(Tlag && !MMe)
           cat("      & 1st-ordered absorption/elim. with lag time\n")
           else if(!Tlag && !MMe)
           cat("      & 1st-ordered absorption/elim. without lag time\n")
           else if(Tlag && MMe)
           cat("      & 1st-ordered absorption, MM elim. with lag time\n")
           else if(!Tlag && MMe)
           cat("      & 1st-ordered absorption, MM elim. without lag time\n")
           cat("      Subject #:", Subject,"                       \n")
           cat("           Dose:", Dose,"                          \n") 
           if(MD) {
           cat("Dosing Interval:", Tau,"                           \n")
           cat("      # of Dose:", nDose,"                         \n")}
           if(Tlag)
           cat("       Lag Time:", Tlag_time,"                      \n")
           else
           cat("       Lag Time: no lag time                        \n")
           cat("     Error Type:", type,"\n\n")     
           if(MMe){
             sim<-matrix(c(ka,Vm,Km,Vd,par1,par2,par3,par4),4,2)
             dimnames(sim)<-list(c("ka","Vm","Km","Vd"),c("Simulated Values","Input Values"))}
           else{
             sim<-matrix(c(ka,kel,Vd,par1,par2,par3),3,2)
             dimnames(sim)<-list(c("ka","kel","Vd"),c("Simulated Values","Input Values"))}
           print(sim, row.names=F)                                                               
           cat("*******************************************************\n\n")      
           
             if (MMe){
                ka<-par1
                Vm<-par2  
                Km<-par3
                Vd<-par4 
                ### PKindex[[i]]<-sfirst.mm.out(PKtime,ka,Vm,Km,Vd,defun,par1,par2,par3,par4,Dose,i,type,MD)      
                time<-PKtime$time
                parms<-c(ka=ka,Vm=Vm,Km=Km,Vd=Vd) 
                C1.lsoda<-data.frame(lsoda(c(Dose,0),c(0,time), defun, parms,rtol=1e-08,atol=1e-08))
                
                good<-ifelse(C1.lsoda[2:(length(time)+1),3]<=1e-5,
                             0,
                             C1.lsoda[2:(length(time)+1),3])
                good<-add.err(good)   ### add random errors to integrated Cp here
                PKindex.this.subj<-data.frame(i,C1.lsoda[2:(length(time)+1),1],good)
                colnames(PKindex.this.subj)<-list("Subject","time","conc")
                print(head(PKindex.this.subj,10), row.names=F);cat("... (the rest of data omitted)\n")   ### since v1.3.7
                x<-C1.lsoda[2:(length(time)+1),1]
                y<-good
                plotting.sim(i,x,y,xaxis,yaxis,MD);PKindex[[i]]<-PKindex.this.subj   ### dump this subj's data to subj i.
             } 
             else{
                ka <-par1
                kel<-par2
                Vd <-par3       
                ### PKindex[[i]]<-sfirst1.out(PKtime,ka,kel,Vd,defun,par1,par2,par3,Dose,i,type,MD)
                time<-PKtime$time
                parms<-c(ka=ka,kel=kel,Vd=Vd)  
                C1.lsoda<-data.frame(lsoda(c(Dose,0),c(0,time), defun, parms,rtol=1e-08,atol=1e-08))  
                
                good<-ifelse(C1.lsoda[2:(length(time)+1),3]<=1e-5,
                             0,
                             C1.lsoda[2:(length(time)+1),3])
                good<-add.err(good)   ### add random errors to integrated Cp here
                PKindex.this.subj<-data.frame(i,C1.lsoda[2:(length(time)+1),1],good)
                colnames(PKindex.this.subj)<-list("Subject","time","conc")
                print(head(PKindex.this.subj,10), row.names=F);cat("... (the rest of data omitted)\n")   ### since v1.3.7
                x<-C1.lsoda[2:(length(time)+1),1]
                y<-good
                plotting.sim(i,x,y,xaxis,yaxis,MD);PKindex[[i]]<-PKindex.this.subj
             }
                save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
          }       
          PKindex<- as.data.frame(do.call("rbind",PKindex))
          rownames(PKindex) <- seq(nrow(PKindex)) 
          ### savefile(PKindex)
       }   
      else {            ### adding errors ####
         if (!MMe){
         par.err<-data.frame(Parameter=c("ka","kel","Vd"),error_factor=c(.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         
         ### dev.new() ### no dev.new() under RStudio! since v1.3.7
         ### par(mfrow=c(2,1),las=1)
         pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
           
             PKindex<-vector(Subject,mode="list")
             pick<- pick-1  # reset pick = 1, 2, 3 or 4 ('0' is zero error & excluded)
             
             for(i in 1:Subject)  {

             ka  <- err_types(pick,par1,factor1)
             kel <- err_types(pick,par2,factor2)
             Vd  <- err_types(pick,par3,factor3)
             
           cat("\n\n     << Subject:- #",i,">>" )
           cat("\n\n")                                                               
           cat("*******************************************************\n")        
           cat(" Summary Table                                       \n\n")   
           if(MD)
           cat(" Model: 1-compartment, extravascular, multiple-dose,  \n")
           else    
           cat(" Model: 1-compartment, extravascular, single-dose,    \n")
           if(Tlag && !MMe)
           cat("      & 1st-ordered absorption/elim. with lag time\n")
           else if(!Tlag && !MMe)
           cat("      & 1st-ordered absorption/elim. without lag time\n")
           else if(Tlag && MMe)
           cat("      & 1st-ordered absorption, MM elim. with lag time\n")
           else if(!Tlag && MMe)
           cat("      & 1st-ordered absorption, MM elim. without lag time\n")
           cat("      Subject #:", Subject,"                       \n")
           cat("           Dose:", Dose,"                          \n") 
           if(MD) {
           cat("Dosing Interval:", Tau,"                           \n")
           cat("      # of Dose:", nDose,"                         \n")}
           if(Tlag)
           cat("       Lag Time:", Tlag_time,"                      \n")
           else
           cat("       Lag Time: no lag time                        \n")
           cat("     Error Type:", type,"\n\n")     
           if(MMe){
             sim<-matrix(c(ka,Vm,Km,Vd,par1,par2,par3,par4),4,2)
             dimnames(sim)<-list(c("ka","Vm","Km","Vd"),c("Simulated Values","Input Values"))}
           else{
             sim<-matrix(c(ka,kel,Vd,par1,par2,par3),3,2)
             dimnames(sim)<-list(c("ka","kel","Vd"),c("Simulated Values","Input Values"))}
           print(sim, row.names=F)                                                               
           cat("*******************************************************\n\n")      
                
           ### PKindex[[i]]<-sfirst1.out(PKtime,ka,kel,Vd,defun,par1,par2,par3,Dose,i,type,MD)  
           time<-PKtime$time
           parms<-c(ka=ka,kel=kel,Vd=Vd)  
           C1.lsoda<-data.frame(lsoda(c(Dose,0),c(0,time), defun, parms,rtol=1e-08,atol=1e-08))  
           
           good<-ifelse(C1.lsoda[2:(length(time)+1),3]<=1e-5,
                        0,
                        C1.lsoda[2:(length(time)+1),3])
           good<-add.err(good)   ### add random errors to integrated Cp here
           PKindex.this.subj<-data.frame(i,C1.lsoda[2:(length(time)+1),1],good)
           colnames(PKindex.this.subj)<-list("Subject","time","conc")
           print(head(PKindex.this.subj,10), row.names=F);cat("... (the rest of data omitted)\n")   ### since v1.3.7
           x<-C1.lsoda[2:(length(time)+1),1]
           y<-good
           plotting.sim(i,x,y,xaxis,yaxis,MD);PKindex[[i]]<-PKindex.this.subj
           save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
             }
          }
      else{   ### if(MMe) here
         par.err<-data.frame(Parameter=c("ka","Vm","Km","Vd"),error_factor=c(.15,.15,.15,.15))
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
             
             for(i in 1:Subject)  {

             ka <- err_types(pick,par1,factor1)
             Vm <- err_types(pick,par2,factor2)
             Km <- err_types(pick,par3,factor3)
             Vd <- err_types(pick,par4,factor4)
             
           cat("\n\n     << Subject:- #",i,">>" )
           cat("\n\n")                                                               
           cat("*******************************************************\n")        
           cat(" Summary Table                                       \n\n")   
           if(MD)
           cat(" Model: 1-compartment, extravascular, multiple-dose,  \n")
           else    
           cat(" Model: 1-compartment, extravascular, single-dose,    \n")
           if(Tlag && !MMe)
           cat("      & 1st-ordered absorption/elim. with lag time\n")
           else if(!Tlag && !MMe)
           cat("      & 1st-ordered absorption/elim. without lag time\n")
           else if(Tlag && MMe)
           cat("      & 1st-ordered absorption, MM elim. with lag time\n")
           else if(!Tlag && MMe)
           cat("      & 1st-ordered absorption, MM elim. without lag time\n")
           cat("      Subject #:", Subject,"                       \n")
           cat("           Dose:", Dose,"                          \n") 
           if(MD) {
           cat("Dosing Interval:", Tau,"                           \n")
           cat("      # of Dose:", nDose,"                         \n")}
           if(Tlag)
           cat("       Lag Time:", Tlag_time,"                      \n")
           else
           cat("       Lag Time: no lag time                        \n")
           cat("     Error Type:", type,"\n\n")     
           if(MMe){
             sim<-matrix(c(ka,Vm,Km,Vd,par1,par2,par3,par4),4,2)
             dimnames(sim)<-list(c("ka","Vm","Km","Vd"),c("Simulated Values","Input Values"))}
           else{
             sim<-matrix(c(ka,kel,Vd,par1,par2,par3),3,2)
             dimnames(sim)<-list(c("ka","kel","Vd"),c("Simulated Values","Input Values"))}
           print(sim, row.names=F)                                                               
           cat("*******************************************************\n\n")      
                
          ### PKindex[[i]]<-sfirst.mm.out(PKtime,ka,Vm,Km,Vd,defun,par1,par2,par3,par4,Dose,i,type,MD)
          time<-PKtime$time
          parms<-c(ka=ka,Vm=Vm,Km=Km,Vd=Vd) 
          C1.lsoda<-data.frame(lsoda(c(Dose,0),c(0,time), defun, parms,rtol=1e-08,atol=1e-08))
          
          good<-ifelse(C1.lsoda[2:(length(time)+1),3]<=1e-5,
                       0,
                       C1.lsoda[2:(length(time)+1),3])
          good<-add.err(good)   ### add random errors to integrated Cp here
          PKindex.this.subj<-data.frame(i,C1.lsoda[2:(length(time)+1),1],good)
          colnames(PKindex.this.subj)<-list("Subject","time","conc")
          print(head(PKindex.this.subj,10), row.names=F);cat("... (the rest of data omitted)\n")   ### since v1.3.7
          x<-C1.lsoda[2:(length(time)+1),1]
          y<-good
          plotting.sim(i,x,y,xaxis,yaxis,MD);PKindex[[i]]<-PKindex.this.subj
          save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE
             } 
         }    
         PKindex<- as.data.frame(do.call("rbind",PKindex))
         rownames(PKindex) <- seq(nrow(PKindex)) 
         ### savefile(PKindex)
     } 
  } 
  else if (pick ==2){    ### starting monte-carlo simulation here 
         choose<-para_err("mc")
         pick<-choose$pick
         type<-choose$type
     
       if (MMe){
         par.err<-data.frame(Parameter=c("ka","Vm","Km","Vd"),error_factor=c(.15,.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         factor4<-par.err[4,2]
       }
       else{
         par.err<-data.frame(Parameter=c("ka","kel","Vd"),error_factor=c(.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         }
         
         cat("\n\nHow many iterations to run for each subject?\n")
         re<-scan(nlines=1,quiet=TRUE)
         sink(zz,split=TRUE)
         set.plot.windows()

         cat("\n\n")                                                               
         cat("*******************************************************\n")        
         cat("Summary Table - Monte-Carlo simulation runs          \n\n")   
         if(MD)
         cat(" Model: 1-compartment, extravascular, multiple-dose,  \n")
         else    
         cat(" Model: 1-compartment, extravascular, single-dose,    \n")
         if(Tlag && !MMe)
         cat("      & 1st-ordered absorption/elim. with lag time\n")
         else if(!Tlag && !MMe)
         cat("      & 1st-ordered absorption/elim. without lag time\n")
         else if(Tlag && MMe)
         cat("      & 1st-ordered absorption, MM elim. with lag time\n")
         else if(!Tlag && MMe)
         cat("      & 1st-ordered absorption, MM elim. without lag time\n")
         cat("      Subject #:", Subject,"                       \n")
         cat("           Dose:", Dose,"                          \n") 
         if(MD) {
         cat("Dosing Interval:", Tau,"                           \n")
         cat("      # of Dose:", nDose,"                         \n")}
         if(Tlag)
         cat("       Lag Time:", Tlag_time,"                      \n")
         else
         cat("       Lag Time: no lag time                        \n")
         cat("   Simulation #:", re,"                            \n\n")
         cat("     Error Type:", type,"\n\n")     
         if(MMe){
           sim<-matrix(c(par1,par2,par3,par4,factor1,factor2,factor3,factor4),4,2)
           dimnames(sim)<-list(c("ka","Vm","Km","Vd"),c("Input Values","Error factor"))}
         else{
           sim<-matrix(c(par1,par2,par3,factor1,factor2,factor3),3,2)
           dimnames(sim)<-list(c("ka","kel","Vd"),c("Input Values","Error factor"))}
         print(par, row.names=F);cat("\n\n")  ### since v1.3.3
         print(sim, row.names=F)                                                               
         cat("*******************************************************")
     
     ### dev.new() ### no dev.new() under RStudio! since v1.3.7
     ### par(mfrow=c(2,1),las=1)
     pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning                  
     
     PKindex<-vector(Subject,mode="list")
     for(i in 1:Subject)  {
       cat("\n\n     << Subject:- #",i,">>\n" )
       C1.lsoda<-list()

         for (j in 1:re){
     
           if (!MMe ){

             ka  <- err_types(pick,par1,factor1)
             kel <- err_types(pick,par2,factor2)
             Vd  <- err_types(pick,par3,factor3)

              time1<-PKtime$time
              parms<-c(ka=ka,kel=kel,Vd=Vd)  
              XX<-data.frame(lsoda(c(Dose,0),c(0,time1), defun, parms,rtol=1e-08,atol=1e-08))
              good<-ifelse(XX[2:(length(time1)+1),3]<=1e-5,
                           0,
                           XX[2:(length(time1)+1),3])
              C1.lsoda[[j]]<-data.frame(XX[2:(length(time1)+1),1],good)
              colnames(C1.lsoda[[j]])<-list("time","concentration") 
           }
           else{   ### if(MMe) here

             ka <- err_types(pick,par1,factor1)
             Vm <- err_types(pick,par2,factor2)
             Km <- err_types(pick,par3,factor3)
             Vd <- err_types(pick,par4,factor4)

              time1<-PKtime$time
              parms<-c(ka=ka,Vm=Vm,Km=Km,Vd=Vd) 
              XX<-data.frame(lsoda(c(Dose,0),c(0,time1), defun, parms,rtol=1e-08,atol=1e-08))
              good<-ifelse(XX[2:(length(time1)+1),3]<=1e-5,
                           0,
                           XX[2:(length(time1)+1),3])
              C1.lsoda[[j]]<-data.frame(XX[2:(length(time1)+1),1],good)
              colnames(C1.lsoda[[j]])<-list("time","concentration") 
           } 
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
   ### if(MD) sone.noniv.route.MD() else sone.noniv.route.SD()
   run(first.run=FALSE)
}