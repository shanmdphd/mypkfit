### Two exponential term
### a one-compartment PK model with the 1st-ordered absorption and 
### the 1st-ordered elmination rate constant  --YJ

fmacro.two<- function(PKindex,
                      A=NULL, 
                      alpha=NULL,
                      ### B=NULL,
                      beta=NULL) 
{
   options(warn=-1)
   modfun<-NULL    ### only exponential terms use 'modfun'
   fm <<- NULL     ### since v1.3.7

   ### since v1.3.8
   fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf
   ###############
           
   ## Input initial value for A, a, B and b
   if (file.exists("fmacro_two.csv") || file.exists("smacro_two.csv")){     ### since v1.3.7
       par.init<-read.par.from.stored(MD,"fmacro_two.csv","smacro_two.csv")}
   else{
       par.init<-data.frame(Parameter=c("A","alpha","beta"),Initial=c(10,0.1,0.5))
       ### par.init<-data.frame(Parameter=c("A","alpha","beta"),Initial=c(10,0.1,20,0.2))
       par.init<-edit(par.init)}
   ### }  ### since v1.3.7
       par.init<-check.para(par.init)
       write.csv(par.init,file="fmacro_two.csv",row.names=FALSE)
       cat("\n")       
       print(par.init, row.names=F)
      
    cat("\n")
    
    modfun<<-function(time,A,alpha,beta){
      pred<-A*(exp(-alpha*time)-exp(-beta*time))
    }
    
    ## Select weighting schemes
    file.menu <-c("equal weight","1/Cp","1/Cp^2")
    pick <- menu(file.menu, title = "<< Weighting Schemes >>")

    with(entertitle(),{
    zz<-before.start.fitting(fit.outputs_to_txt)
    pdf_activate=FALSE
    set.plot.windows()
    
    for( i in 1:length(unique(PKindex$Subject)))  {
      objfun<-function(par){
         out<-modfun(PKindex$time[PKindex$Subject==i],par[1],par[2],par[3])
         gift<- which(PKindex$conc[PKindex$Subject==i]!= 0)
         switch(pick,
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2),
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2/PKindex$conc[gift]),
               sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2))
      }
      
       opt<-optimx(c(par.init[1,2],par.init[2,2],par.init[3,2]),objfun,method="Nelder-Mead",
                   control=list(maxit=5000))             
       nameopt<-c("A","alpha","beta")
       outopt<-c(opt$p1,opt$p2,opt$p3)
       
       if(opt$p1<0) {opt$p1<-0.0001}
       if(opt$p2<0) {opt$p2<-0.0001}
       if(opt$p3<0) {opt$p3<-0.0001}

       weights<-get.weights(PKindex,pick,i)

       fm<-nlsLM(conc~modfun(time,A,alpha,beta),data=subset(PKindex,Subject==i),weights=weights,
                 start=list(A=opt$p1,alpha=opt$p2,beta=opt$p3),
                 control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),lower=c(1e-06,1e-06,1e-06))
       fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
       sink(zz,split=TRUE)
       this.model<-"--- model selection: a two-exponential macroconstant\n\n"
       fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,nameopt,outopt,MMe=FALSE,alpha.beta=TRUE)
       sink()
       save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
      }
       after.fitting(zz,type="fit")  ### since v1.3.7
     })
     run(first.run=FALSE)   
     ### PK.fit(PKindex)
}   
