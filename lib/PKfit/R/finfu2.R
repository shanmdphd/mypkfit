### Two compartment PK model iv infusion single dose    
finfu2<- function(PKindex,
                  Tinf=NULL,
                  Dose=NULL, 
                  kel=NULL,
                  k12=NULL,  
                  k21=NULL,      
                  Vd=NULL,
                  MD=FALSE) 
{
   options(warn=-1)
   modfun<-NULL
   fm <<- NULL   ### since v1.3.7

   ### since v1.3.8
   fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf
   ###############
      
   ## Input dose and Tinf and initial value for kel, k12, k21 and Vd
if(MD){
   if (file.exists("finfu2_md.csv") || file.exists("sinfu2_md.csv")){     ### since v1.3.7
       par.init<-read.par.from.stored(MD,"finfu2_md.csv","sinfu2_md.csv")}
   else{
       par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","Tinf","kel","k12","k21","Vd"),Initial=c(0,0,0,0,0,0,0,0))
       par.init<-edit(par.init)}
   ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        
        Dose <-par.init[1,2]
        Tau  <-par.init[2,2]
        nDose<-par.init[3,2]
        Tinf <-par.init[4,2]
        par1 <-par.init[5,2]
        par2 <-par.init[6,2]
        par3 <-par.init[7,2]
        par4 <-par.init[8,2]

        write.csv(par.init,file="finfu2_md.csv",row.names=FALSE)
        cat("\n")
        print(par.init, row.names=F)
   
   cat("\n")
   
   defun<- function(time, y, parms) { 
       if(time%%Tau<=Tinf)
           dCp1dt<- (Dose/Tinf)/parms["Vd"]+parms["k21"]*y[2]-parms["kel"]*y[1]-parms["k12"]*y[1]
       else 
           dCp1dt<- -parms["kel"]*y[1]-parms["k12"]*y[1]+parms["k21"]*y[2] 
           dCp2dt<- parms["k12"]*y[1]-parms["k21"]*y[2]
       list(c(dCp1dt,dCp2dt)) 
   }
   kel<-par1
   k12<-par2
   k21<-par3
   Vd <-par4
   dosing.time<-seq(0,Tau*nDose,Tau)
   yini<-c(dCp1dt=0,dCp2dt=0)
   modfun <<- function(time,kel,k12,k21,Vd) { 
      out <- lsoda(yini,c(0,time),defun,parms=c(kel=kel,k12=k12,k21=k21,Vd=Vd),rtol=1e-08,atol=1e-08)
      out[-1,2] 
   }
}
else{       #### single-dosed ####
  if (file.exists("finfu2.csv") || file.exists("sinfu2.csv")){     ### since v1.3.7
    par.init<-read.par.from.stored(MD,"finfu2.csv","sinfu2.csv")}
  else{
    par.init<-data.frame(Parameter=c("Dose","Tinf","kel","k12","k21","Vd"),Initial=c(0,0,0,0,0,0))
    par.init<-edit(par.init)}
   ### }  ### since v1.3.7
       par.init<-check.para(par.init)
       write.csv(par.init,file="finfu2.csv",row.names=FALSE)
       cat("\n")
       Dose <- par.init[1,2]
       Tinf <- par.init[2,2]
       par1 <- par.init[3,2]
       par2 <- par.init[4,2]
       par3 <- par.init[5,2]
       par4 <- par.init[6,2]
       print(par.init, row.names=F)
   
   cat("\n")
   
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
   kel <- par1
   k12 <- par2
   k21 <- par3
   Vd  <- par4   
   modfun <<- function(time,kel,k12,k21,Vd) { 
      out <- lsoda(c(0,0),c(0,time),defun,parms=c(kel=kel,k12=k12,k21=k21,Vd=Vd),
                   rtol=1e-6,atol=1e-10) 
      out[-1,2] 
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
        out <- modfun(PKindex$time[PKindex$Subject==i], par[1], par[2], par[3], par[4])
       gift <- which(PKindex$conc[PKindex$Subject==i] != 0)
        ### sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2)
        switch(pick,
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2),
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2/PKindex$conc[gift]),
               sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2))
     }
     
     
     opt <- optimx(c(par1,par2,par3,par4),objfun,method="Nelder-Mead",control=list(maxit=5000))   
     nameopt<-c("kel","k12","k21","Vd")
     outopt<-c(opt$p1,opt$p2,opt$p3,opt$p4)
     
              if(opt$p1<0) {opt$p1<-0.0001}
              if(opt$p2<0) {opt$p2<-0.0001}
              if(opt$p3<0) {opt$p3<-0.0001}
              if(opt$p4<0) {opt$p4<-0.0001}
     
     weights<-get.weights(PKindex,pick,i)
          
     fm<-nlsLM(conc ~ modfun(time,kel,k12,k21,Vd), data=subset(PKindex,Subject==i),
               start=list(kel=opt$p1,k12=opt$p2,k21=opt$p3,Vd=opt$p4),
               control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),weights=weights,
               lower=c(1e-06,1e-06,1e-06,1e-06))
     fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
     sink(zz,split=TRUE)
     this.model<-"--- model selection: two-compartment iv infusion PK model\n    with 1st-ordered elim.\n\n"
     fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                      nameopt,outopt,MMe=FALSE,alpha.beta=FALSE)
     sink()
     save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
     }
     after.fitting(zz,type="fit")  ### since v1.3.7
  })
  run(first.run=FALSE)   
  ### PK.fit(PKindex)   
}