#for three exponential term
smacro.three.out<-function(PKtime,A,alpha,B,beta,C,gamma,defun,par1,par2,par3,
                           par4,par5,par6,i,type,xaxis,yaxis,MD=FALSE)
{         
  time<-PKtime$time
  defun<- A*exp(-alpha*time)+B*exp(-beta*time)+C*exp(-gamma*time)
  ### defun<- defun+runif(length(defun),min=-0.1*defun,max=0.1*defun)  ### add noise to simulated conc.
  defun<-add.err(defun)   ### add random errors to integrated Cp here
  output<-data.frame(time,defun) 
  cat("\n\n")
  cat("******************************************* \n")
  cat(" Summary Table                              \n")
  cat(" Model: three-exponential term              \n")
  cat("   Cp(t)=A*exp(-alpha*t)+B*exp(-beta*t)+    \n")
  cat("         C*exp(-gamma*t)                    \n")
  cat(" Error Type:", type,"                       \n\n")
  sim<-matrix(c(A,alpha,B,beta,C,gamma,par1,par2,par3,par4,par5,par6),6,2)
  dimnames(sim)<-list(c("A","alpha","B","beta","C","gamma"),c("Simulated Values","Input Values"))
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
