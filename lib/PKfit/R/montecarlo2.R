montecarlo2<-function(PKindex,i,re,xaxis,yaxis){
#options(warn=-1)

par(mfrow=c(2,2))
Subj_no<- i
if(Subj_no<10) xfile<-paste("mcsim_subj_0",i,".csv", sep="")
 else          xfile<-paste("mcsim_subj_",i,".csv", sep="")
xx<-read.csv(xfile);file.remove(xfile)       ### remove it; no need any more
UpperLower<-NULL
for(j in 1:length(xx$time)){
   tc<-xx[j,1:re+1];tc<-as.numeric(tc)
   xmean <- mean(tc)
   xsd   <-   sd(tc)
   xN    <-   re
   ### xerr  <- qnorm(0.975)*xsd/sqrt(xN)    ### 95% CI with a normal distribution
   xerr  <- qt(0.975,df=xN-1)*xsd/sqrt(xN)   ### 95% CI with a t distribution
   xleft <- xmean-xerr
   xright<- xmean+xerr
   UpperLower[[j]]<-data.frame(time=xx$time[j],lower=xleft,mean=xmean,upper=xright)
}
xoutput<-as.data.frame(do.call("rbind",UpperLower))

#### if(Subj_no<10) xfile<-paste("95CI_subj_0",Subj_no,".csv", sep="")  ### for testing...
####  else          xfile<-paste("95CI_subj_",Subj_no,".csv", sep="")   ### for testing...
#### write.csv(xoutput,file=xfile,row.names = FALSE)                    ### for testing...

max.conc<-NULL
min.conc<-NULL
MaxMin  <-NULL
for(j in 1:length(xx$time)){
    tc<-xx[j,1:re+1];tc<-as.numeric(tc)
    xmean    <- mean(tc)
    max.conc <-  max(tc)
    min.conc <-  min(tc)
    MaxMin[[j]]<-data.frame(time=xx$time[j],lower=min.conc,mean=xmean,upper=max.conc)
}
moutput<-as.data.frame(do.call("rbind",MaxMin))

x<-PKindex$time
y<-PKindex$conc
meanspline<-spline(xoutput$time,xoutput$mean)    ### smooth the 95%CI lines; look much better! --YJ
xy1spline <-spline(xoutput$time,xoutput$lower)   ### smooth the 95%CI lines
xy2spline <-spline(xoutput$time,xoutput$upper)
main<-paste(c("Subject:-", Subj_no),collapse=" ")
plot(x,y,type='l',col="white",main=main,xlab=xaxis,ylab=yaxis,
     ylim=c(0,1.2*max(xoutput$upper)))
### polygon(c(xoutput$time,rev(xoutput$time)),c(xoutput$lower,rev(xoutput$upper)),border=FALSE,col="grey75")
### lines(xoutput$time,xoutput$mean,type="l",lty=1,col="firebrick3",lwd="2")
polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
lines(meanspline$x,meanspline$y,type="l",lty=1,col="firebrick3",lwd="2")
mtext("linear: shaded area is 95%CI",side=3,cex=0.8)

plot(x,y,type='l',col="white",,log="y",main=main,xlab=xaxis,ylab=yaxis,
     ylim=c(1,2.5*max(xoutput$upper))) ### here ylim must start from 1, not 0; otherwise, the plot does not display.
### polygon(c(xoutput$time,rev(xoutput$time)),c(xoutput$lower,rev(xoutput$upper)),border=FALSE,col="grey75")
### lines(xoutput$time,xoutput$mean,type="l",lty=1,col="firebrick3",lwd="2")
polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
lines(meanspline$x,meanspline$y,type="l",lty=1,col="firebrick3",lwd="2")
mtext("semi-log: shaded area is 95%CI",side=3,cex=0.8)
####
meanspline<-spline(moutput$time,moutput$mean)    ### smooth the 95%CI lines; look much better! --YJ
xy1spline <-spline(moutput$time,moutput$lower)   ### smooth the 95%CI lines
xy2spline <-spline(moutput$time,moutput$upper)
plot(x,y,type='l',col="white",main=main,xlab=xaxis,ylab=yaxis,
     ylim=c(0,1.2*max(moutput$upper)))
### polygon(c(moutput$time,rev(moutput$time)),c(moutput$lower,rev(moutput$upper)),border=FALSE,col="grey75")
### lines(moutput$time,moutput$mean,type="l",lty=1,col="firebrick3",lwd="2")
polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
lines(meanspline$x,meanspline$y,type="l",lty=1,col="firebrick3",lwd="2")
mtext("linear: shaded area is within\n max./min. the dependent",side=3,cex=0.8)

plot(x,y,type='l',col="white",,log="y",main=main,xlab=xaxis,ylab=yaxis,
     ylim=c(1,2.5*max(moutput$upper))) ### here ylim must start from 1, not 0; otherwise, the plot does not display.
### polygon(c(moutput$time,rev(moutput$time)),c(moutput$lower,rev(moutput$upper)),border=FALSE,col="grey75")
### lines(moutput$time,moutput$mean,type="l",lty=1,col="firebrick3",lwd="2")
polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
lines(meanspline$x,meanspline$y,type="l",lty=1,col="firebrick3",lwd="2")
mtext("semi-log: shaded area is within\n max./min. the dependent",side=3,cex=0.8)
####
}