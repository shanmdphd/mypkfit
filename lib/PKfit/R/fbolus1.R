### PKindex is the target Dataset.
### Normal fitting
### One compartment PK model iv bolus single dose
### optional Michaelis-Menten Elimination

fbolus1 <- function(PKindex,
                    Dose=NULL,
                    Vm=NULL,Km=NULL, ## MMe=TRUE
                    Vd=NULL,
                    kel=NULL,        ## MMe=FALSE
                    MMe=FALSE,
                    MD=FALSE)
{
   options(warn=-1)  ### close warnings..  -YJ
   
   modfun1<-NULL
   modfun2<-NULL
   fm <<- NULL   ### since v1.3.7
   
   fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf
   
   ## Input dose and initial value for kel and Vd
if(MD){
   if (MMe){
           if (file.exists("fbolus1_mm_md.csv") || file.exists("sbolus1_mm_md.csv")){     ### since v1.3.7
           par.init<-read.par.from.stored(MD,"fbolus1_mm_md.csv","sbolus1_mm_md.csv")}
           else{
           par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","Vm","Km","Vd"),Initial=c(0,0,0,0,0,0))
           par.init<-edit(par.init)}
           ### }  ### since v1.3.7
           par.init<-check.para(par.init)
           Dose <-par.init[1,2]
           Tau  <-par.init[2,2]
           nDose<-par.init[3,2]
           par1 <-par.init[4,2]
           par2 <-par.init[5,2]
           par3 <-par.init[6,2]
           write.csv(par.init,file="fbolus1_mm_md.csv",row.names=FALSE)
   } 
   else {
      ## No MM elimination
        if (file.exists("fbolus1_md.csv") || file.exists("sbolus1_md.csv")){     ### since v1.3.7
           par.init<-read.par.from.stored(MD,"fbolus1_md.csv","sbolus1_md.csv")}
        else{
        par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","kel","Vd"),Initial_value=c(0,0,0,0,0))
        par.init<-edit(par.init)}
        ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        Dose <-par.init[1,2]
        Tau  <-par.init[2,2]
        nDose<-par.init[3,2]
        par1 <-par.init[4,2]
        par2 <-par.init[5,2]
        write.csv(par.init,file="fbolus1_md.csv",row.names=FALSE)
   }

cat("\n")
print(par.init, row.names=F) 
   
   cat("\n")
   
   if (!MMe) {
      kel<-par1 ### req.
      Vd <-par2 ### req.
      ## User-supplied function w/o Michaelis-Mention elimination
      defun <- function(time, y, parms) {
      if(time==0) dCpdt <- Dose/parms["Vd"]   ### the key code to success for iv bolus -multiple-dosed. 
      else dCpdt <- -parms["kel"] * y[1] 
      list(dCpdt) 
      } 
      dosing.time<-seq(Tau,Tau*nDose,Tau)
      yini<-c(dCpdt=Dose/Vd)
      events <- data.frame(var="dCpdt",time=dosing.time,value=Dose/Vd,method="add")
      modfun1 <<- function(time,kel, Vd) {  
      out <- lsoda(yini,c(0,time),defun,parms=c(kel=kel,Vd=Vd),
                   events=list(data=events),rtol=1e-06,atol=1e-10) 
      out[-1,2]    ### to extract conc. value at column# 2; and delete the first row (repeated (0,dose))
      }
   } 
   else {
      ## User-supplied function with MM elimination
      Vm <-par1 ### req.
      Km <-par2 ### req.
      Vd <-par3 ### req.
      defun<- function(time, y, parms) { 
      if(time==0) dCpdt <- Dose/parms["Vd"]   ### the key code to success for iv bolus -multiple-dosed. 
      else dCpdt <- -parms["Vm"]*y[1]/(parms["Km"]+y[1]) 
      list(dCpdt)
      }
      dosing.time<-seq(Tau,Tau*nDose,Tau)
      yini<-c(dCpdt=Dose/Vd)
      events <- data.frame(var="dCpdt",time=dosing.time,value=Dose/Vd,method="add")
      modfun2 <<- function(time,Vm,Km,Vd) { 
      out <- lsoda(yini,c(0,time),defun,parms=c(Vm=Vm,Km=Km,Vd=Vd),
                   events=list(data=events),rtol=1e-06,atol=1e-10)
      out[-1,2]    ### to extract conc. value at column# 2; and delete the first row (repeated (0,dose))
      }
   }
}
else{                                #### single-dosed ####
   if (MMe){
           if (file.exists("fbolus1_mm.csv") || file.exists("sbolus1_mm.csv")){     ### since v1.3.7
           par.init<-read.par.from.stored(MD,"fbolus1_mm.csv","sbolus1_mm.csv")}
           else{
           par.init<-data.frame(Parameter=c("Dose","Vm","Km","Vd"),Initial=c(0,0,0,0))
           par.init<-edit(par.init)}
           ### }  ### since v1.3.7
           par.init<-check.para(par.init)
           Dose <-par.init[1,2]
           par1 <-par.init[2,2]
           par2 <-par.init[3,2]
           par3 <-par.init[4,2]
           write.csv(par.init,file="fbolus1_mm.csv",row.names=FALSE)
   } 
   else {
      ## No MM elimination
        if (file.exists("fbolus1.csv") || file.exists("sbolus1.csv")){     ### since v1.3.7
           par.init<-read.par.from.stored(MD,"fbolus1.csv","sbolus1.csv")}
        else{
        par.init<-data.frame(Parameter=c("Dose","kel","Vd"),Initial_value=c(0,0,0))
        par.init<-edit(par.init)}
        ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        Dose <-par.init[1,2]
        par1 <-par.init[2,2]
        par2 <-par.init[3,2]
        write.csv(par.init,file="fbolus1.csv",row.names=FALSE)      
   }

cat("\n")  
print(par.init, row.names=F) 
   
   cat("\n")
   
   if (!MMe) {
      ## User-supplied function w/o Michaelis-Mention elimination
      defun <- function(time, y, parms) { 
      dCpdt <- -parms["kel"] * y[1] 
      list(dCpdt) 
      } 
      kel<- par1
      Vd <- par2
      modfun1 <<- function(time,kel, Vd) {  
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(kel=kel,Vd=Vd),
                   rtol=1e-6,atol=1e-10) 
      out[-1,2]    ### to extract conc. value at column# 2; and delete the first row (repeated (0,dose))
      }
   } 
   else {
      ## User-supplied function with MM elimination
      defun<- function(time, y, parms) { 
      dCpdt <- -parms["Vm"]*y[1]/(parms["Km"]+y[1]) 
      list(dCpdt)
      }
      Vm<- par1
      Km<- par2
      Vd<- par3
      modfun2 <<- function(time,Vm,Km,Vd) { 
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(Vm=Vm,Km=Km,Vd=Vd),rtol=1e-08,atol=1e-08)
      out[-1,2]     ### to extract conc. value at column# 2; and delete the first row (repeated (0,dose))
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
           out <- modfun2(PKindex$time[PKindex$Subject==i], par[1], par[2],par[3])
        } 
        else {
           ## No MM elimination
           out <- modfun1(PKindex$time[PKindex$Subject==i], par[1], par[2])
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
         opt<-optimx(c(par1,par2,par3),objfun,method="Nelder-Mead",control=list(maxit=5000))
         nameopt<-c("Vm","Km","Vd")
         outopt<-c(opt$p1,opt$p2,opt$p3)
        }
        else {
         opt<-optimx(c(par1,par2),objfun,method="Nelder-Mead",control=list(maxit=5000))  
         nameopt<-c("kel","Vd")
         outopt<-c(opt$p1,opt$p2)
        }
        
        if (MMe){
               if(opt$p1<0) {opt$p1<-0.0001}
               if(opt$p2<0) {opt$p2<-0.0001}
               if(opt$p3<0) {opt$p3<-0.0001}
        }
        else {
               if(opt$p1<0) {opt$p1<-0.0001}
               if(opt$p2<0) {opt$p2<-0.0001}
        }      
     
     weights<-get.weights(PKindex,pick,i)

     if (MMe) {
         fm<-nlsLM(conc~modfun2(time,Vm,Km,Vd),data=subset(PKindex,Subject==i),
                   start=list(Vm=opt$p1,Km=opt$p2,Vd=opt$p3),weights=weights,
                   control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),
                   lower=c(1e-06,1e-06,1e-06))
         fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
         sink(zz,split=TRUE)
         this.model<-"--- model selection: a one-compartment, iv bolus pk model\n    with M-M elim.\n\n"
         fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                          nameopt,outopt,MMe,alpha.beta=FALSE)
         sink()
         save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
       } 
       else {
        ## No MM elimination
         fm<-nlsLM(conc ~ modfun1(time, kel, Vd),data=subset(PKindex,Subject==i),start=list(kel=opt$p1,Vd=opt$p2),
                   weights=weights,control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),lower=c(1e-06,1e-06))
         fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
         sink(zz,split=TRUE)
         this.model<-"--- model selection: a one-compartment, iv bolus pk model\n    with 1st-ordered elim.\n\n"
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
