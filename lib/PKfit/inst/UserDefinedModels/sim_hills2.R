
### for PKfit v1.3.7 or above
### Sigmoid Emax Response versus conc. (Hill Equation)
### 
require(PKfit)
### require(deSolve)   ### not req. since there is no ode in this model
OutputFilez()
options(warn=-1)
options(width=100)     ### for output width
graphics.off()

Subject =NULL   # N Subj's 
PKtime  =NULL   # this becomes conc. (not time) instead now! (CR)
Emax    =NULL
EC50    =NULL
gamma   =NULL
E0      =NULL
MD      =FALSE

options(warn=-1)
### since v1.3.7
run_on_RStudio<-NULL
if(.Platform$GUI=="RStudio") {run_on_RStudio<-TRUE;plot.new();dev.off()} else
{run_on_RStudio<-FALSE}
if(run_on_RStudio){sim.outputs_to_txt<-sim.outputs_to_txt} else
{sim.outputs_to_txt<-sim.outputs_to_txt;sim.plots_to_pdf<-sim.plots_to_pdf}
###############
xaxis <- "drug plasma conc."
yaxis <- "simulated response"

#### output function for simulation
hills_sim.out<-function(PKtime,Emax,EC50,gamma,E0,defun,par1,par2,par3,par4,i,type,xaxis,yaxis,MD)
{
  time<-PKtime$time
  conc<-time  ### treat 'time' as 'conc' here
  defun<- E0 + Emax*conc^gamma/(EC50^gamma+conc^gamma)
  defun<-add.err(defun)   ### add random errors to integrated Cp here
  output<-data.frame(time,defun)
  cat("\n\n")
  cat("**************************************************\n")
  cat(" Summary Table                                \n")
  cat(" Model: Sigmoid Emax Response vs. Conc.       \n")
  cat("   Response=E0 + Emax*conc^gamma/(EC50^gamma+conc)\n")
  cat(" Error Type:", type,"                         \n\n")
  sim<-matrix(c(Emax,EC50,gamma,E0,par1,par2,par3,par4),4,2)
  dimnames(sim)<-list(c("Emax","EC50","gamma","E0"),c("Simulated Values","Input Values"))
  print(sim, row.names=F)
  cat("**************************************************\n\n")
  PKindex <- data.frame(i,output)
  PKindex2<- PKindex   ### dump to the other dataset for print correctly
  colnames(PKindex) <-list("Subject","time","conc")
  colnames(PKindex2)<-list("subject","conc","response")
  ### print(PKindex, row.names=F)  ### original code
  print(PKindex2, row.names=F)
  x<-PKindex[,2]
  y<-PKindex[,3]
  plotting.sim(i,x,y,xaxis,yaxis,MD)
  return(PKindex) 
}
####
montecarlo_hills<-function(C1.lsoda,time1,i,re,xaxis,yaxis){
#options(warn=-1)

  all.C1.lsoda<-C1.lsoda    ### reserve the original 'C1.lsoda' first
  conc<-rowMeans(as.data.frame(lapply(C1.lsoda,"[","concentration")))
  C1.lsoda<-do.call("rbind",C1.lsoda)
  rownames(C1.lsoda)<-seq(nrow(C1.lsoda))
  
  conc<-ifelse(conc<=0,0,conc)
  PKindex <-data.frame(i,time1,conc)
  par(mfrow=c(2,2))
  x<-C1.lsoda$time
  
  y<-ifelse(C1.lsoda$concentration<=0,0,C1.lsoda$concentration)
  Subj_no<-i
  main<-paste(c("Subject:-", i),collapse=" ")
  plot(C1.lsoda,type="p",main=main,xlab=xaxis,ylab=yaxis)      ### ylim cannot be used here!
  lines(PKindex$time1,PKindex$conc,type="l",lty=1,col="firebrick3",lwd="2")
  mtext("Linear",side=3,cex=0.8)
  
  plot(x,y,log="y",type='p',main=main,xlab=xaxis,ylab=yaxis)  ### ylim cannot be used here!
  lines(PKindex$time1,PKindex$conc,log="y",type="l",lty=1,col="firebrick3",lwd="2")
  mtext("Semi-log",side=3,cex=0.8) 
  
  len<-length(time1)
  sub<-len*re
  a<-seq(0,sub,by=len)
  AA<-a[2:(length(a)-1)]
  colnames(PKindex)<-list("Subject","time","conc")
  PKindex2 <- PKindex
  colnames(PKindex2)<-list("subject","drug conc.","response")
  ### cat("\n");print(head(PKindex,12), row.names=F);cat("      ... (the rest of data omitted)")  ### original code
  print(head(PKindex2,12), row.names=F);cat("      ... (the rest of data omitted)")
  
  for(i in rev(AA))
      C1.lsoda<-C1.lsoda[append(1:(sub+(length(AA)-1)),NA,after=i),]   
  
  plot(C1.lsoda,type="l",main=main,xlab=xaxis,ylab=yaxis)    ### ylim cannot be used here!
  lines(PKindex$time,PKindex$conc,type="l",lty=1,col="firebrick3",lwd="2")
  mtext("Linear",side=3,cex=0.8)
  
  x<-C1.lsoda$time
  y<-C1.lsoda$conc
  
  plot(x,y,log="y",type='l',main=main,xlab=xaxis,ylab=yaxis)
  lines(PKindex$time,PKindex$conc,log="y",type="l",lty=1,col="firebrick3",lwd="2")  ### 'log="y" here may not be req.
  mtext("Semi-log",side=3,cex=0.8)
  
#### since v1.3.3 to plot band and mean of conc. -> montecarlo2() takes over
if(Subj_no<10) xfile<-paste("mcsim_subj_0",Subj_no,".csv", sep="")
 else          xfile<-paste("mcsim_subj_", Subj_no,".csv", sep="")
xx<-as.data.frame(all.C1.lsoda) ### 'all.C1.lsoda' is not originally a data.frame!
j<-3;for(i in 1:(re-1)){xx<-xx[,-j];j<- j+1} ### delete 'time.1', 'time.2'... here
write.csv(xx,file=xfile,row.names=FALSE)
return(PKindex)
}
####
cat("\n First enter all paramaters used for simulation profile.\n");readline(" Press Enter to continue...\n\n")
if(file.exists("hills_sim2.csv")){
   par<-read.csv(file="hills_sim2.csv",row.names=NULL,header=TRUE)
   par<-edit(par)} else{ 
   par<-data.frame(Parameter=c("Total_subject#","Emax","EC50","gamma","E0"),Initial=c(24,11.6,0.12,0.51,1.0))
   par<-edit(par)
   }
   par<-check.para(par)
   write.csv(par,file="hills_sim2.csv",row.names=FALSE) 
   cat("\n")       
   print(par, row.names=F)
   
   cat("\n")
   Subject<-par[1,2]
      par1<-par[2,2]
      par2<-par[3,2]
      par3<-par[4,2]
      par4<-par[5,2]

   readline("\n Next enter or load time points. Press Enter to continue.\n\n")
   if(file.exists("hills_sim_conc2.csv")){
      PKtime<-read.csv(file="hills_sim_conc2.csv",row.names=NULL,header=TRUE)
      PKtime<-PKtime$time  ### convert to numeric
      PKtime<-edit(PKtime)
      PKtime<-as.data.frame(PKtime);colnames(PKtime)<-"time"}  else{
      PKtime<-c(0,.1,.2,.3,.4,.6,.8,1,2,4,6,8,12,14,16,18,24,48,72)  ### change these if necessary
      PKtime<-edit(PKtime)
      PKtime<-as.data.frame(PKtime);colnames(PKtime)<-"time"}
   
   write.csv(PKtime,file="hills_sim_conc2.csv",row.names=FALSE)
   print(PKtime, row.names=F);cat("\n\n")
   cat("\n")
    
    file.menu <- c("Simulation with Error",
                   "Monte-Carlo Simulation")
    pick <- menu(file.menu, title = "<< Simulation Types >>") 

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
          ### dev.new()
          par(mfrow=c(2,1),las=1)
          pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
          Subject<-1          ### does not make any sense to simulate more than 1 subject with this option.
          PKindex<-vector(Subject,mode="list")
          for(i in 1:Subject)  {
            cat("\n\n     << Subject:- #",i,">>" )
            Emax  <-par1
            EC50  <-par2  
            gamma <-par3
            E0    <-par4
            PKindex[[i]]<-hills_sim.out(PKtime,Emax,EC50,gamma,E0,defun,par1,par2,par3,par4,i,type,xaxis,yaxis,MD)
            if(!run_on_RStudio) {save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE}  ### since v1.3.7
            }  
      PKindex<- as.data.frame(do.call("rbind",PKindex))
      rownames(PKindex) <- seq(nrow(PKindex)) 
      ### savefile(PKindex)  
      }  else {
         par.err<-data.frame(Parameter=c("Emax","EC50","gamma","E0"),error_factor=c(.15,.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         if(.Platform$OS.type=="windows" && .Platform$GUI=="Rgui") {windows(record=TRUE)} ### since v1.3.7
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         factor4<-par.err[4,2]
         
         ### dev.new()
         par(mfrow=c(2,1),las=1)
         pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning
         
         PKindex<-vector(Subject,mode="list")
         pick<- pick-1  # reset pick = 1, 2, 3 or 4 ('0' is zero error & excluded)
         
         for( i in 1:Subject)  { 

             Emax  <- err_types(pick,par1,factor1)
             EC50  <- err_types(pick,par2,factor2)
             gamma <- err_types(pick,par3,factor3)
             E0    <- err_types(pick,par4,factor4)
                      
            cat("\n\n       << Subject:- #",i,">>" )    ### used to implement. -YJ
            PKindex[[i]]<-hills_sim.out(PKtime,Emax,EC50,gamma,E0,defun,par1,par2,par3,par4,i,type,xaxis,yaxis,MD)
            if(!run_on_RStudio) {save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE}  ### since v1.3.7
            }   
         PKindex<- as.data.frame(do.call("rbind",PKindex))
         rownames(PKindex) <- seq(nrow(PKindex)) 
         ### savefile(PKindex)     
        } 
      }  else if (pick ==2){ ### starting monte-carlo sim here
         choose<-para_err("mc")
         pick<-choose$pick
         type<-choose$type
      
         par.err<-data.frame(Parameter=c("Emax","EC50","gamma","E0"),error_factor=c(.15,.15,.15,.15))
         par.err<-edit(par.err)
         par.err<-check.para(par.err)
         
         factor1<-par.err[1,2]
         factor2<-par.err[2,2]
         factor3<-par.err[3,2]
         factor4<-par.err[4,2]
         
      cat("\n\nHow many iterations to run for each subject?\n")
      re<-scan(nlines=1,quiet=TRUE)
      if(.Platform$OS.type=="windows" && .Platform$GUI=="Rgui") {windows(record=TRUE)} ### since v1.3.7
      sink(zz,split=TRUE)
      
      cat("\n\n")
      cat("****************************************************\n")
      cat(" Summary Table (Monte-Carlo simulation runs)\n")
      cat(" Model: Sigmoid Emax Response vs. Conc.       \n")
      cat("   Response = E0 + Emax*conc^gamma/(EC50^gamma+conc)\n")
      cat(" Subject #:", Subject,"            \n")
      cat(" Error Type:", type,"              \n")
      cat(" Simulation #:", re,"              \n\n")
      sim<-matrix(c(par1,par2,par3,par4,factor1,factor2,factor3,factor4),4,2)
      dimnames(sim)<-list(c("Emax","EC50","gamma","E0"),c("Input Values","Error factor"))
      print(par, row.names=F);cat("\n\n")  ### since v1.3.3
      print(sim, row.names=F)   
      cat("****************************************************")

      ### dev.new()
      par(mfrow=c(2,1),las=1)
      pdf_activate=FALSE  ### set pdf device activate? as FALSE at beginning

      PKindex<-vector(Subject,mode="list")
      for( i in 1:Subject)  {
        cat("\n\n   << Subject:- #",i," >>\n" )    ### used to implement. -YJ
        C1.lsoda<-list()
           for (j in 1:re){
           
             Emax  <- err_types(pick,par1,factor1)
             EC50  <- err_types(pick,par2,factor2)
             gamma <- err_types(pick,par3,factor3)
             E0    <- err_types(pick,par4,factor4)
                       
              time1<-PKtime$time
              conc<-time1   ### treat 'time' as 'conc' here
              defun<- E0 + Emax*conc^gamma/(EC50^gamma+conc^gamma)
              XX<-data.frame(conc,defun) 
              C1.lsoda[[j]]<-data.frame(XX$conc,XX$defun) 
              colnames(C1.lsoda[[j]])<-list("time","concentration")
           }
            PKindex[[i]]<-montecarlo_hills(C1.lsoda,conc,i,re,xaxis,yaxis) 
            if(!run_on_RStudio) {save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE}  ### since v1.3.7
            montecarlo2(PKindex[[i]],i,re,xaxis,yaxis)        ### since v1.3.3
            if(!run_on_RStudio) {save.plots.as.pdf(sim.plots_to_pdf,pdf_activate);pdf_activate=TRUE}  ### since v1.3.7
         }
    PKindex<-as.data.frame(do.call("rbind",PKindex))
    rownames(PKindex)<-seq(nrow(PKindex)) 
    ### savefile(PKindex)  
   }
   colnames(PKindex)<-c("subject","conc","response")
   savefile(PKindex)
   after.fitting(zz,run_on_RStudio,type="sim")  ### since v1.3.7
   ### run()  ### this line is definitely req. when running under RStudio!
   ### cat("... end of run.")
### end of codes
