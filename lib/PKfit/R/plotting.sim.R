### ----------------plot for simulation----------------
### Draw 2 windows, and consider that 2 more windows are coming
plotting.sim <- function(i,x,y,xaxis,yaxis,MD=FALSE) 
{

### dev.new()
### par(mfrow=c(2,1),ask = FALSE) ### moved to each module since v1.3.7

main<-paste(c("Subject:- ", i),collapse="")
   
if(MD){
  ## linear plot
  plot(y~x,type='l',main=main,ylim=c(0,max(y)*1.1),     ### plot lines only('l'ine only)
       xlab=xaxis,ylab=yaxis,pch=15,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
  text("linear plot",side=3,cex=0.88)

  ## semi-log plot
  plot(x,y,log="y",type='l',main=main,ylim=c(1,max(y)*1.1),
       xlab=xaxis,ylab=yaxis,pch=15,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
  mtext("semi-log plot",side=3,cex=0.88) 
}
else{
  ## linear plot
  plot(y~x,type='b',main=main,ylim=c(0,max(y)*1.1),    ### plot lines and symbols ('b'oth)
       xlab=xaxis,ylab=yaxis,pch=15,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
  text("linear plot",side=3,cex=0.88)

  ## semi-log plot
  plot(x,y,log="y",type='b',main=main,ylim=c(1,max(y)*1.1),  
  ### plot(x,y,log="y",type='l',main=main,
       xlab=xaxis,ylab=yaxis,pch=15,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
  mtext("semi-log plot",side=3,cex=0.88) 
}
}
