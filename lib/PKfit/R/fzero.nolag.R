### PKindex is the target Dataset.
### Normal fitting
### One compartment PK model extravascular single 
### dose zero-order absorption without lag time (absorption rate, Ro = Dose/Tabs)
### optional Michaelis-Menten Elimination

fzero.nolag <- function(PKindex,
                        Dose=NULL, 
                        Tabs=NULL,       ## zero-ordered absorption time
                        Vm=NULL,Km=NULL, ## MMe=TRUE
                        Vd=NULL,
                        kel=NULL,        ## MMe=FALSE
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
           
   ## Input dose and initial value for Tabs, kel and Vd
if(MD){
   if (MMe){
      if (file.exists("fzero_nolag_mm_md.csv") || file.exists("szero_nolag_mm_md.csv")){     ### since v1.3.7
        par.init<-read.par.from.stored(MD,"fzero_nolag_mm_md.csv","szero_nolag_mm_md.csv")}
      else{
        par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","Tabs","Vm","Km","Vd"),Initial=c(0,0,0,0,0,0,0))
        par.init<-edit(par.init)}
      ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        Dose  <- par.init[1,2]
        Tau   <- par.init[2,2]
        nDose <- par.init[3,2]
        par1  <- par.init[4,2]
        par2  <- par.init[5,2]
        par3  <- par.init[6,2]
        par4  <- par.init[7,2]
        write.csv(par.init,file="fzero_nolag_mm_md.csv",row.names=FALSE)
   } 
   else {
      ## No MM elimination
      if (file.exists("fzero_nolag_md.csv") || file.exists("szero_nolag_md.csv")){     ### since v1.3.7
        par.init<-read.par.from.stored(MD,"fzero_nolag_md.csv","szero_nolag_md.csv")}
        else{
        par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","Tabs","kel","Vd"),Initial=c(0,0,0,0,0,0))
        par.init<-edit(par.init)}
      ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        Dose  <-par.init[1,2]
        Tau   <-par.init[2,2]
        nDose <-par.init[3,2]
        par1  <-par.init[4,2]
        par2  <-par.init[5,2]
        par3  <-par.init[6,2]
        write.csv(par.init,file="fzero_nolag_md.csv",row.names=FALSE)
   }
        cat("\n")
        print(par.init, row.names=F);cat("\n")
   
   if (!MMe) {
      ## User-supplied function w/o Michaelis-Mention elimination
      defun<- function(time, y, parms) { 
         if(time<Tau){                  ### for this 1st dose. --YJ
         if(time<=parms["Tabs"]) 
            dCpdt <- (Dose/parms["Tabs"])/parms["Vd"] - parms["kel"] * y[1]
         else
            dCpdt <- - parms["kel"] * y[1]
         }
         else{                         ### for 2nd, 3rd,.... dose. -YJ
         if(time%%Tau<=1 && time%%Tau<=parms["Tabs"]) 
            dCpdt <- (Dose/parms["Tabs"])/parms["Vd"] - parms["kel"] * y[1]
         else dCpdt <- - parms["kel"] * y[1]
         }
         list(dCpdt) 
      } 
      Tabs<-par1
      kel <-par2
      Vd  <-par3 
      modfun1 <<- function(time,Tabs,kel,Vd) { 
         out <- lsoda(0,c(0,time),defun,parms=c(Tabs=Tabs,kel=kel,Vd=Vd),rtol=1e-08,atol=1e-08) 
         out[-1,2]
      } 
   } 
   else {
     ## User-supplied function with MM elimination
      defun<- function(time, y, parms) { 
        if(time<Tau){
        if(time<=parms["Tabs"]) 
          dCpdt <- (Dose/parms["Tabs"])/parms["Vd"]-parms["Vm"]*y[1]/(parms["Km"]+y[1])            
        else
          dCpdt <- -parms["Vm"]*y[1]/(parms["Km"]+y[1])
        }
        else{
        if(time%%Tau<=1 && time%%Tau<=parms["Tabs"])
          dCpdt <- (Dose/parms["Tabs"])/parms["Vd"]-parms["Vm"]*y[1]/(parms["Km"]+y[1])            
        else
          dCpdt <- -parms["Vm"]*y[1]/(parms["Km"]+y[1]) 
        }          
        list(dCpdt)
      }   
      Tabs<-par1
      Vm  <-par2
      Km  <-par3
      Vd  <-par4
      modfun2 <<- function(time,Tabs,Vm,Km,Vd) { 
      out <- lsoda(0,c(0,time),defun,parms=c(Tabs=Tabs,Vm=Vm,Km=Km,Vd=Vd),rtol=1e-08,atol=1e-08) 
      out[-1,2]
      } 
    }
}
else{             #### single-dosed from here ####
   if (MMe){
      if (file.exists("fzero_nolag_mm.csv") || file.exists("szero_nolag_mm.csv")){     ### since v1.3.7
        par.init<-read.par.from.stored(MD,"fzero_nolag_mm.csv","szero_nolag_mm.csv")}
      else{
        par.init<-data.frame(Parameter=c("Dose","Tabs","Vm","Km","Vd"),Initial=c(0,0,0,0,0))
        par.init<-edit(par.init)}
      ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        Dose <- par.init[1,2]
        par1 <- par.init[2,2]
        par2 <- par.init[3,2]
        par3 <- par.init[4,2]
        par4 <- par.init[5,2]
        write.csv(par.init,file="fzero_nolag_mm.csv",row.names=FALSE)
   } 
   else {
      ## No MM elimination
      if (file.exists("fzero_nolag.csv") || file.exists("szero_nolag.csv")){     ### since v1.3.7
        par.init<-read.par.from.stored(MD,"fzero_nolag.csv","szero_nolag.csv")}
      else{
        par.init<-data.frame(Parameter=c("Dose","Tabs","kel","Vd"),Initial=c(0,0,0,0))
        par.init<-edit(par.init)}
      ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        Dose <- par.init[1,2]
        par1 <- par.init[2,2]
        par2 <- par.init[3,2]
        par3 <- par.init[4,2]
        write.csv(par.init,file="fzero_nolag.csv",row.names=FALSE)
   }
        cat("\n")
        print(par.init, row.names=F);cat("\n")
   
   if (!MMe) {
      ## User-supplied function w/o Michaelis-Mention elimination
      defun<- function(time, y, parms) { 
      if(time<=parms["Tabs"]) 
        dCpdt <- (Dose/parms["Tabs"])/parms["Vd"] - parms["kel"] * y[1]
      else
        dCpdt <- - parms["kel"] * y[1]
      list(dCpdt) 
      } 
      Tabs <- par1
      kel  <- par2
      Vd   <- par3
      modfun1 <<- function(time,Tabs,kel,Vd) { 
         out <- lsoda(0,c(0,time),defun,parms=c(Tabs=Tabs,kel=kel,Vd=Vd),
                      rtol=1e-08,atol=1e-08) 
         out[-1,2]
      } 
   } 
   else {
     ## User-supplied function with MM elimination
      defun<- function(time, y, parms) { 
      if(time<=parms["Tabs"]) 
        dCpdt <- (Dose/parms["Tabs"])/parms["Vd"]-(parms["Vm"]/parms["Vd"])*y[1]/(parms["Km"]/parms["Vd"]+y[1])            
      else
        dCpdt <- -(parms["Vm"]/parms["Vd"])*y[1]/(parms["Km"]/parms["Vd"]+y[1])            
      list(dCpdt) 
      }   
      Tabs <- par1
      Vm   <- par2
      Km   <- par3
      Vd   <- par4
      modfun2 <<- function(time,Tabs,Vm,Km,Vd) { 
      out <- lsoda(0,c(0,time),defun,parms=c(Tabs=Tabs,Vm=Vm,Km=Km,Vd=Vd),
                   rtol=1e-6,atol=1e-10) 
      out[-1,2]
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
           out<-modfun2(PKindex$time[PKindex$Subject==i],par[1],par[2],par[3],par[4])
        } 
        else {
           ## No MM elimination
           out<-modfun1(PKindex$time[PKindex$Subject==i],par[1],par[2],par[3])
        }
           gift <- which(PKindex$conc[PKindex$Subject==i] != 0)
           ### sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2)
           switch(pick,
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2),
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2/PKindex$conc[gift]),
               sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2))
      }
      
###      
     if (MMe) {
        opt<-optimx(c(par1,par2,par3,par4),objfun,method="Nelder-Mead",control=list(maxit=5000))  
        nameopt<-c("Tabs","Vm","Km","Vd")
        outopt<-c(opt$p1,opt$p2,opt$p3,opt$p4)
     }
     else {
        opt<-optimx(c(par1,par2,par3),objfun,method="Nelder-Mead",control=list(maxit=5000))  
        nameopt<-c("Tabs","kel","Vd")
        outopt<-c(opt$p1,opt$p2,opt$p3)
     }
     
      if (MMe){
              if(opt$p1<0) {opt$p1<-0.0001}
              if(opt$p2<0) {opt$p2<-0.0001}
              if(opt$p3<0) {opt$p3<-0.0001}
              if(opt$p4<0) {opt$p4<-0.0001}
       }
       else {
              if(opt$p1<0) {opt$p1<-0.0001}
              if(opt$p2<0) {opt$p2<-0.0001}
              if(opt$p3<0) {opt$p3<-0.0001}
       }      
     
     weights<-get.weights(PKindex,pick,i)
     
     if (MMe) {
        fm<-nlsLM(conc~modfun2(time,Tabs,Vm,Km,Vd),data=subset(PKindex,Subject==i),
                  start=list(Tabs=opt$p1,Vm=opt$p2,Km=opt$p3,Vd=opt$p4),
                  control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),weights=weights,
                  lower=c(1e-06,1e-06,1e-06,1e-06))
        fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
        sink(zz,split=TRUE)
        this.model<-"--- model selection: a one-compartment pk model with\n    zero-ordered abs., M-M elim.\n\n"
        fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                         nameopt,outopt,MMe,alpha.beta=FALSE)
        sink()
        save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
     } 
     else {
        ## No MM elimination
        fm<-nlsLM(conc~modfun1(time,Tabs,kel,Vd),data=subset(PKindex,Subject==i),
                  start=list(Tabs=opt$p1,kel=opt$p2,Vd=opt$p3),
                  control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),weights=weights,
                  lower=c(1e-06,1e-06,1e-06))
        fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
        sink(zz,split=TRUE)
        this.model<-"--- model selection: a one-compartment pk model with\n    zero-ordered abs., 1st-ordered elim.\n\n"
        fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                         nameopt,outopt,MMe,alpha.beta=FALSE)
        sink()
        save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
     }
  }
        after.fitting(zz,type="fit")  ### since v1.3.7
  })
  run(first.run=FALSE)   
  ### PK.fit(PKindex)
}