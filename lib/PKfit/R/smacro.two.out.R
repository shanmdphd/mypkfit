#for two exponential term
smacro.two.out<-function(PKtime,A,alpha,beta,defun,par1,par2,par3,i,type,xaxis,yaxis,MD=FALSE)
{
  time<-PKtime$time
  defun<- A*(exp(-alpha*time)-exp(-beta*time))
  ### defun<- defun+runif(length(defun),min=-0.1*defun,max=0.1*defun)  ### add noise to simulated conc.
  defun<-add.err(defun)   ### add random errors to integrated Cp here
  output<-data.frame(time,defun)
  cat("\n\n")
  cat("******************************************* \n")
  cat(" Summary Table                              \n")
  cat(" Model: two-exponential term                \n")
  cat("   Cp(t)=A*(exp(-alpha*t))-exp(-beta*t))    \n")
  cat(" Error Type:", type,"                       \n\n")
  sim<-matrix(c(A,alpha,beta,par1,par2,par3),3,2)
  dimnames(sim)<-list(c("A","alpha","beta"),c("Simulated Values","Input Values"))
  print(sim, row.names=F)
  cat("*******************************************\n\n")
  PKindex<-data.frame(i,output)
  colnames(PKindex)<-list("Subject","time","conc")
  print(head(PKindex.this.subj,10), row.names=F);cat("... (the rest of data omitted)\n")   ### since v1.3.7
  x<-PKindex[,2]
  y<-PKindex[,3]
  plotting.sim(i,x,y,xaxis,yaxis,MD)
  return(PKindex) 
}
