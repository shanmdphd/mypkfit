### Normal fitting
### One compartment PK model extravascualr single dose first-order absorption 
### Without lag time --> simulation code (sfirst.nolog())

ffirst.nolag<- function(PKindex,
                        Dose=NULL, 
                        ka=NULL,
                        Vm=NULL,Km=NULL, 
                        Vd=NULL,
                        kel=NULL,        
                        Tlag=FALSE,
                        MMe=FALSE,
                        MD=FALSE) 
{
   options(warn=-1)
   modfun1<-NULL
   modfun2<-NULL
   fm <<- NULL   ### since v1.3.7
        
   ### since v1.3.7
   run_on_RStudio<-NULL
   if(.Platform$GUI=="RStudio") {run_on_RStudio<-TRUE;plot.new();dev.off()} else
     {run_on_RStudio<-FALSE}
   if(run_on_RStudio){fit.outputs_to_txt<-fit.outputs_to_txt} else
     {fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf}
   ###############
           
   ## Input dose and Tlag and initial value for ka, kel and Vd
if(MD){
   if (MMe){
     if (file.exists("ffirst_nolag_mm_md.csv") || file.exists("sfirst_nolag_mm_md.csv")){     ### since v1.3.7
     par.init<-read.par.from.stored(MD,"ffirst_nolag_mm_md.csv","sfirst_nolag_mm_md.csv")}
     else{
     par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","ka","Vm","Km","Vd"),Initial=c(0,0,0,0,0,0,0))
     par.init<-edit(par.init)}
     ### }  ### since v1.3.7
     par.init<-check.para(par.init)
     
     Dose <-par.init[1,2]
     Tau  <-par.init[2,2]
     nDose<-par.init[3,2]
     par1 <-par.init[4,2]
     par2 <-par.init[5,2]
     par3 <-par.init[6,2]
     par4 <-par.init[7,2]
     
     write.csv(par.init,file="ffirst_nolag_mm_md.csv",row.names=FALSE)
   } 
   else{
     if (file.exists("ffirst_nolag_md.csv") || file.exists("sfirst_nolag_md.csv")){     ### since v1.3.7
       par.init<-read.par.from.stored(MD,"ffirst_nolag_md.csv","sfirst_nolag_md.csv")}
     else{
       par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","ka","kel","Vd"),Initial=c(0,0,0,0,0,0))
       par.init<-edit(par.init)}
     ### }  ### since v1.3.7}
       par.init<-check.para(par.init)
       
       Dose <-par.init[1,2]
       Tau  <-par.init[2,2]
       nDose<-par.init[3,2]
       par1 <-par.init[4,2]
       par2 <-par.init[5,2]
       par3 <-par.init[6,2]
       
       write.csv(par.init,file="ffirst_nolag_md.csv",row.names=FALSE)
   }
        cat("\n")
        print(par.init, row.names=F);cat("\n")
   
   if (MMe) {
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
      ka <-par1
      Vm <-par2  
      Km <-par3
      Vd <-par4   
      modfun2 <<- function(time,ka,Vm,Km,Vd) { 
      out <- lsoda(c(Dose,0),c(0,time),defun,parms=c(ka=ka,Vm=Vm,Km=Km,Vd=Vd),rtol=1e-08,atol=1e-08) 
      out[-1,3] 
      } 
   } 
   else {
      ## User-supplied function w/o MM elimination w/o lag time
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
      ka <-par1
      kel<-par2
      Vd <-par3 
      modfun1 <<- function(time,ka,kel,Vd) { 
      out <- lsoda(c(Dose,0),c(0,time),defun,parms=c(ka=ka,kel=kel,Vd=Vd),rtol=1e-08,atol=1e-08) 
      out[-1,3] 
      }
   }
}                ##### single-dosed ####
else{
   if (MMe){
    if (file.exists("ffirst_nolag_mm.csv") || file.exists("sfirst_nolag_mm.csv")){     ### since v1.3.7
      par.init<-read.par.from.stored(MD,"ffirst_nolag_mm.csv","sfirst_nolag_mm.csv")}
    else{
      par.init<-data.frame(Parameter=c("Dose","ka","Vm","Km","Vd"),Initial=c(0,0,0,0,0))
      par.init<-edit(par.init)}
    ### }  ### since v1.3.7
      par.init<-check.para(par.init)
      Dose <- par.init[1,2]
      par1 <- par.init[2,2]
      par2 <- par.init[3,2]
      par3 <- par.init[4,2]
      par4 <- par.init[5,2]
      write.csv(par.init,file="ffirst_nolag_mm.csv",row.names=FALSE)
   } 
   else{
      if (file.exists("ffirst_nolag.csv") || file.exists("sfirst_nolag.csv")){     ### since v1.3.7
        par.init<-read.par.from.stored(MD,"ffirst_nolag.csv","sfirst_nolag.csv")}
      else{
        par.init<-data.frame(Parameter=c("Dose","ka","kel","Vd"),Initial=c(0,0,0,0))
        par.init<-edit(par.init)}
      ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        Dose <- par.init[1,2]
        par1 <- par.init[2,2]
        par2 <- par.init[3,2]
        par3 <- par.init[4,2]        
        write.csv(par.init,file="ffirst_nolag.csv",row.names=FALSE)
   }
        cat("\n")
        print(par.init, row.names=F);cat("\n")
   
   if (MMe) {
      ## User-supplied function with MM elimination w/o lag time
      defun<- function(time, y, parms) { 
      dy1dt <- -parms["ka"] * y[1]
      dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["Vm"]*y[2]/(parms["Km"]+y[2])
      list(c(dy1dt,dy2dt)) 
      } 
      ka  <- par1
      Vm  <- par2
      Km  <- par3
      Vd  <- par4
      modfun2 <<- function(time,ka,Vm,Km,Vd) { 
      out <- lsoda(c(Dose,0),c(0,time),defun,parms=c(ka=ka,Vm=Vm,Km=Km,Vd=Vd),rtol=1e-08,atol=1e-08) 
      out[-1,3] 
      } 
   } 
   else {
      ## User-supplied function w/o MM elimination w/o lag time
      defun <- function(time, y, parms) { 
      dy1dt <- -parms["ka"] * y[1]
      dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["kel"] * y[2]
      list(c(dy1dt,dy2dt)) 
      } 
      ka  <- par1
      kel <- par2
      Vd  <- par3
      modfun1 <<- function(time,ka,kel,Vd) { 
      out <- lsoda(c(Dose, 0),c(0,time),defun,parms=c(ka=ka,kel=kel,Vd=Vd),rtol=1e-08,atol=1e-08) 
      out[-1,3] 
      }
   }
}
   ## Select weighting schemes
   file.menu <-c("equal weight","1/Cp","1/Cp^2")
   pick <- menu(file.menu, title = "<< Weighting Schemes >>")

   with(entertitle(),{
   zz<-before.start.fitting(fit.outputs_to_txt)
   pdf_activate=FALSE
   set.plot.windows()

   for( i in 1:length(unique(PKindex$Subject)))  {
      objfun <- function(par) {
        if (MMe) {
           out <- modfun2(PKindex$time[PKindex$Subject==i], par[1], par[2],par[3],par[4]) 
        } 
        else  {
           out <- modfun1(PKindex$time[PKindex$Subject==i], par[1], par[2], par[3])
        }
        gift <- which(PKindex$conc[PKindex$Subject==i] != 0)
        ### sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2)
        switch(pick,
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2),
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2/PKindex$conc[gift]),
               sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2))
     }

      if (MMe) {
        opt<-optimx(c(par1,par2,par3,par4),objfun,method="Nelder-Mead",control=list(maxit=5000))
        nameopt<-c("ka","Vm","Km","Vd")
        outopt<-c(opt$p1,opt$p2,opt$p3,opt$p4)
      }
      else {
        opt<-optimx(c(par1,par2,par3),objfun,method="Nelder-Mead",control=list(maxit=5000))
        nameopt<-c("ka","kel","Vd")
        outopt<-c(opt$p1,opt$p2,opt$p3)
      }
           
      if (MMe){
              if(opt$p1<0) {opt$p1<-0.001}
              if(opt$p2<0) {opt$p2<-0.001}
              if(opt$p3<0) {opt$p3<-0.001}
              if(opt$p4<0) {opt$p4<-0.001}
       }
       else {
              if(opt$p1<0) {opt$p1<-0.001}
              if(opt$p2<0) {opt$p2<-0.001}
              if(opt$p3<0) {opt$p3<-0.001}
       }
      
      weights<-get.weights(PKindex,pick,i)
           
      if (MMe) {
        fm <<- NULL
        fm<-nlsLM(conc ~ modfun2(time, ka, Vm, Km, Vd), data=subset(PKindex,Subject==i),
                  start=list(ka=opt$p1,Vm=opt$p2,Km=opt$p3,Vd=opt$p4),weights=weights,
                  control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),lower=c(1e-06,1e-06,1e-06,1e-06))
        fm <<- fm
        sink(zz,split=TRUE)
        this.model<-"--- model selection: a one-compartment pk model with\n    1st no lag-time abs., M-M elim.\n\n"
        fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                         nameopt,outopt,MMe,alpha.beta=FALSE)
        sink()
        save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
      } 
      else {
        ## when not an MM elimination
        fm <-nlsLM(conc ~ modfun1(time, ka, kel, Vd), data=subset(PKindex,Subject==i),
             start=list(ka=opt$p1,kel=opt$p2,Vd=opt$p3),weights=weights,
             control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),lower=c(1e-06,1e-06,1e-06))
        sink(zz,split=TRUE)
        this.model<-"--- model selection: a one-compartment pk model with\n    1st no lag-time abs./elim.\n\n"
        fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                         nameopt,outopt,MMe,alpha.beta=FALSE)
        sink()
        save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
      }
   }
        after.fitting(zz,type="fit")  ### since v1.3.7
   })
  run(first.run=FALSE)   
} 
