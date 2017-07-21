### PKindex is the target Dataset.


### Normal fitting
### One exponential term

fmacro.one <- function(PKindex,
                       A=NULL,
                       a=NULL) 
{
   options(warn=-1)
   modfun<-NULL    ### only exponential terms use 'modfun'
   fm <<- NULL     ### since v1.3.7

   ### since v1.3.8
   fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf
   ###############
      
   ## Input and initial value for A and a
   
   if (file.exists("fmacro_one.csv") || file.exists("smacro_one.csv")){     ### since v1.3.7
       par.init<-read.par.from.stored(MD,"fmacro_one.csv","smacro_one.csv")}
   else{
       par.init<-data.frame(Parameter=c("A","alpha"),Initial=c(0,0))
       ### par.init<-data.frame(Parameter=c("A","alpha"),Initial=c(10,0.1))
       par.init<-edit(par.init)}
   ### }  ### since v1.3.7
       par.init<-check.para(par.init)
       write.csv(par.init,file="fmacro_one.csv",row.names=FALSE)
       cat("\n")       
       print(par.init, row.names=F)
   
   cat("\n")
   
   modfun<<-function(time,A,alpha){
     pred<- A*exp(-alpha*time)
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
         out<-modfun(PKindex$time[PKindex$Subject==i],par[1],par[2])
         gift<- which( PKindex$conc[PKindex$Subject==i]!= 0)
         switch(pick,
                sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2),
                sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2/PKindex$conc[gift]),
                sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2)
                )
      }
      
       opt<-optim(c(par.init[1,2],par.init[2,2]),objfun,method="Nelder-Mead",control=list(maxit=5000))       
       nameopt<-c("A","alpha")
       outopt<-c(opt$par[1],opt$par[2])
      
            if(opt$par[1]<0) {opt$par[1]<-0.01}
            if(opt$par[2]<0) {opt$par[2]<-0.01}

       weights<-get.weights(PKindex,pick,i)
              
       fm<-nlsLM(conc~modfun(time,A,alpha),data=subset(PKindex,Subject==i),
                 start=list(A=opt$par[1],alpha=opt$par[2]),
                 weights=weights, control=nls.lm.control(maxiter=500))
       fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
       sink(zz,split=TRUE)
       this.model<-"--- model selection: a one-exponential macroconstant\n\n"
       fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,nameopt,outopt,MMe=FALSE,alpha.beta=TRUE)
       sink()
       save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
      }
       after.fitting(zz,type="fit")  ### since v1.3.7
     })
  run(first.run=FALSE)   
}      
