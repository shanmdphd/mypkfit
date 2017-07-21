montecarlo<-function(C1.lsoda,time1,i,re,xaxis,yaxis){
#options(warn=-1)

  all.C1.lsoda<-C1.lsoda    ### reserve the original 'C1.lsoda' first
  conc<-rowMeans(as.data.frame(lapply(C1.lsoda,"[","concentration")))
  C1.lsoda<-do.call("rbind",C1.lsoda)
  rownames(C1.lsoda)<-seq(nrow(C1.lsoda))
  
  conc<-ifelse(conc<=0,0,conc)
  PKindex <-data.frame(i,time1,conc)
  par(mfrow=c(2,2))
  x<-C1.lsoda$time
  
  y<-ifelse(C1.lsoda$concentration<=0,0,C1.lsoda$concentration)
  Subj_no<-i
  main<-paste(c("Subject:-", i),collapse=" ")
  plot(C1.lsoda,type="p",main=main,xlab=xaxis,ylab=yaxis)      ### ylim cannot be used here!
  lines(PKindex$time1,PKindex$conc,type="l",lty=1,col="firebrick3",lwd="2")
  mtext("Linear",side=3,cex=0.8)
  
  plot(x,y,log="y",type='p',main=main,xlab=xaxis,ylab=yaxis)  ### ylim cannot be used here!
  lines(PKindex$time1,PKindex$conc,log="y",type="l",lty=1,col="firebrick3",lwd="2")
  mtext("Semi-log",side=3,cex=0.8) 
  
  len<-length(time1)
  sub<-len*re
  a<-seq(0,sub,by=len)
  AA<-a[2:(length(a)-1)]
  colnames(PKindex)<-list("Subject","time","conc")
  print(head(PKindex,12), row.names=F);cat("      ... (the rest of data omitted)")
  
  for(i in rev(AA))
      C1.lsoda<-C1.lsoda[append(1:(sub+(length(AA)-1)),NA,after=i),]   
  
  plot(C1.lsoda,type="l",main=main,xlab=xaxis,ylab=yaxis)    ### ylim cannot be used here!
  lines(PKindex$time,PKindex$conc,type="l",lty=1,col="firebrick3",lwd="2")
  mtext("Linear",side=3,cex=0.8)
  
  x<-C1.lsoda$time
  y<-C1.lsoda$conc
  
  plot(x,y,log="y",type='l',main=main,xlab=xaxis,ylab=yaxis)
  lines(PKindex$time,PKindex$conc,log="y",type="l",lty=1,col="firebrick3",lwd="2")  ### 'log="y" here may not be req.
  mtext("Semi-log",side=3,cex=0.8)
  
#### since v1.3.3 to plot band and mean of conc. -> montecarlo2() takes over
if(Subj_no<10) xfile<-paste("mcsim_subj_0",Subj_no,".csv", sep="")
 else          xfile<-paste("mcsim_subj_", Subj_no,".csv", sep="")
xx<-as.data.frame(all.C1.lsoda) ### 'all.C1.lsoda' is not originally a data.frame!
j<-3;for(i in 1:(re-1)){xx<-xx[,-j];j<- j+1} ### delete 'time.1', 'time.2'... here
write.csv(xx,file=xfile,row.names=FALSE)
####
return(PKindex)
}