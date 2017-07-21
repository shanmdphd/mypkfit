fit.final.output<-function(PKindex,i,fm,this.model,pick,par.init,xaxis,yaxis,
                           nameopt,outopt,MMe=FALSE,alpha.beta=FALSE){
                           
if(MMe==FALSE && alpha.beta==FALSE) coef<-data.frame(coef(fm)["kel"])
if(MMe==FALSE && alpha.beta==TRUE)  coef<-data.frame(coef(fm)["alpha"])
cat(" ********************************\n\n")
cat("      --- Subject:- #",i,"---    \n\n")
cat(" ********************************\n\n")
cat("--- input data ---\n")
conc<-PKindex$conc[PKindex$Subject==i]
time<-PKindex$time[PKindex$Subject==i]
this_subj<-data.frame(time, conc)
print(this_subj, row.names=F);cat("\n")     # show input data    
cat("--- initial values for parameters ---\n")
print(par.init, row.names=F);cat("\n\n")    # show initial values here
cat("--- weighting scheme: ")             ## show weighting scheme
switch(pick,cat("equal weight"),cat("1/Cp"),cat("1/Cp^2"));cat("\n\n")
cat(this.model)       
cat("<< PK parameter obtained from Nelder-Mead Simplex algorithm >>\n\n")
print(data.frame(Parameter=nameopt,Value=outopt), row.names=F);cat("\n")     
if(MMe) plotting.non(PKindex, fm, i, pick, xaxis, yaxis)
  else  plotting.lin(PKindex, fm, i, pick, coef, xaxis, yaxis)
}