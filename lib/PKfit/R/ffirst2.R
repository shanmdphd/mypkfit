### Two compartment PK model extravascular single dose first order absorption
ffirst2<- function(PKindex,
                  ka=NULL,
                  Dose=NULL, 
                  kel=NULL,
                  k12=NULL,  
                  k21=NULL,      
                  Vd=NULL,
                  MD=FALSE)          ### added multiple-dose
{
   options(warn=-1)
   modfun<-NULL
   fm <<- NULL   ### since v1.3.7
   
   fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf
   
if(MD){
   ## Input dose and initial value for ka, kel, k12, k21 and Vd
        if (file.exists("ffirst2_md.csv") || file.exists("sfirst2_md.csv")){     ### since v1.3.7
           par.init<-read.par.from.stored(MD,"ffirst2_md.csv","sfirst2_md.csv")}
        else{
        par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","ka","kel","k12","k21","Vd"),Initial=c(0,0,0,0,0,0,0,0))
        ### par.init<-data.frame(Parameter=c("Dose","Tau","#Dose","ka","kel","k12","k21","Vd"),Initial=c(300,8,10,0.51,0.36,0.13,0.31,11.7))
        par.init<-edit(par.init)}
        ### }  ### since v1.3.7
        par.init<-check.para(par.init)
        write.csv(par.init,file="ffirst2_md.csv", row.names=F)    ### delete 'col.names=T'; otherwise error
        cat("\n")
        Dose <-par.init[1,2]
        Tau  <-par.init[2,2]
        nDose<-par.init[3,2]
        par1 <-par.init[4,2]
        par2 <-par.init[5,2]
        par3 <-par.init[6,2]
        par4 <-par.init[7,2]
        par5 <-par.init[8,2]
        print(par.init, row.names=F)
   
   cat("\n")
   
   ka  <- par1
   kel <- par2
   k12 <- par3
   k21 <- par4
   Vd  <- par5
   defun<- function(time, y, parms) { 
     dCp1dt <- -parms["ka"]*y[1]
     dCp2dt <-  parms["ka"]*y[1]/parms["Vd"]-parms["kel"]*y[2]+parms["k21"]*y[3]-parms["k12"]*y[2]
     dCp3dt <-  parms["k12"]*y[2]-parms["k21"]*y[3]
     list(c(dCp1dt,dCp2dt,dCp3dt)) 
   } 
   dosing.time<-seq(Tau,Tau*nDose,Tau)                   ### this and next two lines are req. for MD
   yini<-c(dCp1dt=Dose,dCp2dt=0,dCp3dt=0)
   events <- data.frame(var="dCp1dt",time=dosing.time,value=Dose,method="add")
   modfun <<- function(time,ka,kel,k12,k21,Vd) { 
   out <- lsode(yini,c(0,time),defun,parms=c(ka=ka,kel=kel,k12=k12,k21=k21,Vd=Vd), ### lsoda() and lsode() same
                events=list(data=events),rtol=1e-08,atol=1e-08)                    ### add events() for MD
   out[-1,3] 
   }
}
else{                                   ### single-dose ####

   ## Input dose and initial value for ka, kel, k12, k21 and Vd
     if (file.exists("ffirst2.csv") || file.exists("sfirst2.csv")){  ### since v1.3.7
        par.init<-read.par.from.stored(MD,"ffirst2.csv","sfirst2.csv")}
     else{
        par.init<-data.frame(Parameter=c("Dose","ka","kel","k12","k21","Vd"),Initial=c(0,0,0,0,0,0))
        ### par.init<-data.frame(Parameter=c("Dose","ka","kel","k12","k21","Vd"),Initial=c(300,0.3,0.2,0.3,0.4,20))
        par.init<-edit(par.init)}
        ### } ### since v1.3.7
        par.init<-check.para(par.init)
        write.csv(par.init,file="ffirst2.csv", row.names=F)    ### delete 'col.names=T'; otherwise error
        cat("\n")
        Dose <-par.init[1,2]
        par1 <-par.init[2,2]
        par2 <-par.init[3,2]
        par3 <-par.init[4,2]
        par4 <-par.init[5,2]
        par5 <-par.init[6,2]  
        print(par.init, row.names=F)
   
   cat("\n")
   
   defun<- function(time, y, parms) { 
     dCp1dt <- -parms["ka"]*y[1]
     dCp2dt <-  parms["ka"]*y[1]/parms["Vd"]-parms["kel"]*y[2]+parms["k21"]*y[3]-parms["k12"]*y[2]
     dCp3dt <-  parms["k12"]*y[2]-parms["k21"]*y[3]
     list(c(dCp1dt,dCp2dt,dCp3dt)) 
   } 
   ka  <- par1
   kel <- par2
   k12 <- par3
   k21 <- par4
   Vd  <- par5
   modfun <<- function(time,ka,kel,k12,k21,Vd) { 
     out <- lsoda(c(Dose,0,0),c(0,time),defun,parms=c(ka=ka,kel=kel,k12=k12,k21=k21,Vd=Vd),
                  rtol=1e-08,atol=1e-08) 
     out[-1,3] 
   }
}   
   ## Select weighting schemes
   file.menu <-c("equal weight","1/Cp","1/Cp^2")
   pick <- menu(file.menu, title = "<< Weighting Schemes >>")

   with(entertitle(),{
   zz<-before.start.fitting(fit.outputs_to_txt)
   pdf_activate=FALSE
   set.plot.windows()

for(i in 1:length(unique(PKindex$Subject)))  {
     objfun <- function(par) {
        out <- modfun(PKindex$time[PKindex$Subject==i],par[1],par[2],par[3],par[4],par[5])  ### calc. Ct function
       gift <- which(PKindex$conc[PKindex$Subject==i] != 0)                                 ### all non-zero Ct 
       ### sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2)
       switch(pick,
              sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2),                      ### WRSS (equal wt)
              sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2/PKindex$conc[gift]),   ### WRSS (1/Ct)
              sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2)) ### WRSS (1/Ct^2)
 }

     opt<-optimx(c(par1,par2,par3,par4,par5),objfun,method="Nelder-Mead",control=list(maxit=5000))
     nameopt<-c("ka","kel","k12","k21","Vd")
     outopt<-c(opt$p1,opt$p2,opt$p3,opt$p4,opt$p5)
      
     if(opt$p1<0) {opt$p1<-0.0001}
     if(opt$p2<0) {opt$p2<-0.0001}
     if(opt$p3<0) {opt$p3<-0.0001}
     if(opt$p4<0) {opt$p4<-0.0001}
     if(opt$p5<0) {opt$p5<-0.0001}
     
     weights<-get.weights(PKindex,pick,i)
     fm<-nlsLM(conc ~ modfun(time,ka,kel,k12,k21,Vd),data=subset(PKindex,Subject==i),
               start=list(ka=opt$p1,kel=opt$p2,k12=opt$p3,k21=opt$p4,Vd=opt$p5),
               control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),
               weights=weights,lower=c(1e-06,1e-06,1e-06,1e-06,1e-06))
     fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
     sink(zz,split=TRUE)
     this.model<-"--- model selection: a two-compartment, extravascular pk model with\n    1st-ordered abs./elim.\n\n"
     fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                      nameopt,outopt,MMe=FALSE,alpha.beta=FALSE)
     sink()
     save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
     }
     after.fitting(zz,type="fit")  ### since v1.3.7
  })
  run(first.run=FALSE)   
}