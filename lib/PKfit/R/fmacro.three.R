### Three exponential term
### this script seems not very stable when fitting with simulated dataset...?
### don't know why?
fmacro.three<- function(PKindex,
                        A=NULL, 
                        alpha=NULL,
                        B=NULL,
                        beta=NULL,
                        C=NULL,
                        gamma=NULL) 
{
   options(warn=-1)
   modfun<-NULL    ### only exponential terms use 'modfun'
   fm <<- NULL   ### since v1.3.7

   ### since v1.3.7
   fit.outputs_to_txt<-fit.outputs_to_txt;fit.plots_to_pdf<-fit.plots_to_pdf
   ###############
           
   ## Input initial value for A, a, B, b, C and c
   if (file.exists("fmacro_three.csv") || file.exists("smacro_three.csv")){     ### since v1.3.7
       par.init<-read.par.from.stored(MD,"fmacro_three.csv","smacro_three.csv")}
   else{
       par.init<-data.frame(Parameter=c("A","alpha","B","beta","C","gamma"),Initial=c(0,0,0,0,0,0))
       ### par.init<-data.frame(Parameter=c("A","alpha","B","beta","C","gamma"),Initial=c(10,0.1,20,0.2,30,0.3))
       par.init<-edit(par.init)}
   ### }  ### since v1.3.7
       par.init<-check.para(par.init)
       write.csv(par.init,file="fmacro_three.csv",row.names=FALSE)
       cat("\n")       
       print(par.init, row.names=F)
      
    cat("\n")
    
    modfun<<-function(time,A,alpha,B,beta,C,gamma){
      pred<- A*exp(-alpha*time)+B*exp(-beta*time)+C*exp(-gamma*time)
    }  
    
    ## Select weighting schemes
    file.menu <-c("equal weight","1/Cp","1/Cp^2")
    pick <- menu(file.menu, title = "<< Weighting Schemes >>")

    with(entertitle(),{
    zz<-before.start.fitting(fit.outputs_to_txt)
    pdf_activate=FALSE
    set.plot.windows()
    
    for(i in 1:length(unique(PKindex$Subject)))  {
     objfun<-function(par) {
         out<-modfun(PKindex$time[PKindex$Subject==i],par[1],par[2],par[3],par[4],par[5],par[6])
         gift<- which(PKindex$conc[PKindex$Subject==i]!= 0)
         switch(pick,
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2),
               sum((PKindex$conc[PKindex$Subject==i][gift]-out[gift])^2/PKindex$conc[gift]),
               sum(((PKindex$conc[PKindex$Subject==i][gift]-out[gift])/PKindex$conc[gift])^2))
      }
      
        opt<-optimx(c(par.init[1,2],par.init[2,2],par.init[3,2],par.init[4,2],par.init[5,2],par.init[6,2]),
                    objfun,method="Nelder-Mead",control=list(maxit=5000))  
        nameopt<-c("A","alpha","B","beta","C","gamma")
        outopt<-c(opt$p1,opt$p2,opt$p3,opt$p4,opt$p5,opt$p6)
        
                if(opt$p1<0) {opt$p1<-0.0001}
                if(opt$p2<0) {opt$p2<-0.0001}
                if(opt$p3<0) {opt$p3<-0.0001}
                if(opt$p4<0) {opt$p4<-0.0001}
                if(opt$p5<0) {opt$p5<-0.0001}
                if(opt$p6<0) {opt$p6<-0.0001}

        weights<-get.weights(PKindex,pick,i)
          
        fm<-nlsLM(conc~modfun(time,A,alpha,B,beta,C,gamma),data=subset(PKindex,Subject==i),weights=weights,
                  start=list(A=opt$p1,alpha=opt$p2,B=opt$p3,beta=opt$p4,C=opt$p5,gamma=opt$p6),
                  control=nls.lm.control(maxiter=500,maxfev=5000,factor=100),
                  lower=c(1e-06,1e-06,1e-06,1e-06,1e-06,1e-06))
        fm <<- fm   ### otherwise, when plotting the shade of 95% predicted fitted values, the error can occur (cannot find 'fm'); since v1.3.7 -YJ
        sink(zz,split=TRUE)
        this.model<-"--- model selection: a three-exponential macroconstant\n\n"
        fit.final.output(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,nameopt,outopt,MMe=FALSE,alpha.beta=TRUE)
        sink()
        save.plots.as.pdf(fit.plots_to_pdf,pdf_activate);pdf_activate=TRUE  ### since v1.3.7
      }
        after.fitting(zz,type="fit")  ### since v1.3.7
     })
  run(first.run=FALSE)
  ### PK.fit(PKindex)
}   
