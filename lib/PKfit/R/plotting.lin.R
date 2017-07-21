#---------------plot for linear----------------
plotting.lin <- function (PKindex,fm,i,pick,coef,xaxis,yaxis,separateWindows=TRUE){

  ### options(warn=-1)
  ### requireNamespace("nls2", quietly=TRUE)  ### since R v3.1.3 with RTools v3.3; nls2 must be in th list of 'Depends'
  ### par(ask = FALSE)  ###  this is default
  j<-1:length(PKindex$time[PKindex$Subject==i])
  x<-PKindex$time[PKindex$Subject==i]
  y<-PKindex$conc[PKindex$Subject==i]
    
 #Weighted residuals   
 if (!(pick %in% 1:3)) {
    stop("Pick is illegal")
  }

### Calculated concentration & weighted residuals
      ### cal2<-predict(fm,list(time=x))
      cal<-fitted(fm)   ### same as above
  
      ### wei2 <- switch(pick,
      ###                ifelse(y[j]==0.0, 0, y[j]-cal[j]),
      ###                ifelse(y[j]==0.0, 0, sqrt(1/(y[j]))*(y[j]-cal[j])),
      ###                ifelse(y[j]==0.0, 0, sqrt(1/((y[j])^2))*(y[j]-cal[j])))
      
      wei<- switch(pick,               ### same as above
                   ifelse(y[j]==0.0, 0, residuals(fm)),
                   ifelse(y[j]==0.0, 0, residuals(fm)*sqrt(1/(y[j]))),
                   ifelse(y[j]==0.0, 0, residuals(fm)*sqrt(1/((y[j])^2))))
  
 #calculate AUC and AUMC   
  add<-function(time,conc){
     auc<-0 ; aumc<-0
     for(i in 2:length(time)) {
     auc[i]<-1/2*(time[i]-time[i-1])*(conc[i]+conc[i-1])
     auc[i]<-auc[i]+auc[i-1]
     aumc[i]<-1/2*(time[i]-time[i-1])*(conc[i]*time[i]+conc[i-1]*time[i-1])
     aumc[i]<-aumc[i]+aumc[i-1]
     }
     return(list(auc=auc,aumc=aumc))
  }
  add1<-add(x,y)
  if (x[1]==0){
    AUC<-add1$auc
    AUMC<-add1$aumc
    }
  else {
  AUC<-c(NA,add1$auc[-1])
  AUMC<-c(NA,add1$aumc[-1])
  }
        
 #Output   
  cat("<< Residual sum-of-squares and final PK parameters values with nlsLM >>\n\n")
  output<-data.frame(x,y,cal,wei,AUC,AUMC)
  if(pick==1)
     colnames(output)<-list("Time","Observed","Calculated","Residuals","AUC","AUMC")
  else
     colnames(output)<-list("Time","Observed","Calculated","Wt. Residuals","AUC","AUMC")
  ### print(fm,row.names=F);cat("\n");print(confint(fm),row.names=F);cat("\n");print(output,row.names=F)
  print(fm,row.names=F);cat("\n");print(output,row.names=F)
  
 #AUC (0 to infinity)              
  ### cat(paste(c("\n<< AUC (0 to",max(y),"computed by trapezoidal rule >>\n\n"))
  auc.infinity<-y[length(y)]/coef[1,1]
  auc<-AUC[length(y)]+auc.infinity
  ### print(auc) 
  
 #AUMC (0 to infinity) 
  ### cat(paste(c("\n<< AUMC (0 to",max(y),"computed by trapezoidal rule >>\n\n")
  aumc.infinity<-(x[length(x)]*y[length(y)])/coef[1,1]+y[length(y)]/((coef[1,1])^2)
  aumc<-AUMC[length(y)]+aumc.infinity
  ### print(aumc)
  sumNCA<-data.frame(Parameters=c("Cmax","Cmin","AUC0-inf","AUMC0-inf"),
                     values=c(max(y),min(y),auc,aumc))
  cat("\n");print(sumNCA, row.names=F)
         
  aicllsbc(fm)
  cat("\n")

### if(.Platform$OS.type=="windows" && .Platform$GUI=="Rgui") windows(record=TRUE)  ### will failed to save plots to pdf!
### if(.Platform$GUI=="RStudio") run_on_RStudio <- TRUE
### dev.new() ### no dev.new() under RStudio! since v1.3.7
par(mfrow=c(2,2),ask = FALSE)   ### remove this remark for final release....  ???  -YJ
main<-paste(c("Subject#:- ", i),collapse=" ")
j<-1:length(PKindex$time[PKindex$Subject==i])
xstep<-seq(from=min(PKindex$time[PKindex$Subject==i]),
             to=max(PKindex$time[PKindex$Subject==i]),by=0.01)  ### to smooth the line
x<-PKindex$time[PKindex$Subject==i]
y<-PKindex$conc[PKindex$Subject==i]
Ymax<-max(max(predict(fm,list(time=xstep))),max(y))
fittedline<-data.frame(time=xstep,conc_fit=predict(fm,list(time=xstep)))

if(packageVersion('proto')=="0.3.10"){
### plot shaded 95%CI of the predicted values for each time point
### Bonate_P_Pharmacokinetic_Pharmacodynamic_Modeling_and_Simulation_2nd, p.122
xx<-predict(as.lm.nls(fm),list(time=xstep),interval="confidence",level=0.95)
xx<-as.data.frame(xx)
# show(xx)
### xx$lwr<-ifelse(xx$lwr<=0,1e-06,xx$lwr);xx$upr<-ifelse(xx$upr<=0,1e-06,xx$upr)        ### cannot be zero or negative for semi-log plot
xy1spline <-spline(x,xx$lwr) ### ;xy1spline$y<-ifelse(xy1spline$y<=0,1e-06,xy1spline$y)  ### smooth the 95%CI lines
xy2spline <-spline(x,xx$upr) ### ;xy2spline$y<-ifelse(xy2spline$y<=0,1e-06,xy2spline$y)
###
### Linear plot
plot(y~x,data=PKindex,type='p',main=main,ylim=c(0,Ymax*1.2),
     xlab=xaxis, ylab=yaxis,pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
#### polygon(c(x,rev(x)),c(xx$lwr,rev(xx$upr)),border=FALSE,col="grey75")      ### only for linear plots
polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
points(x,y,pch=15,col="black")
lines(xstep,predict(fm,list(time=xstep)),type="l",lty=1,col="firebrick3",lwd="2")
mtext("linear plot",side=3,cex=0.88)
} else {  
### not solved for proto v1.0.0 yet...  since v1.3.7
### Linear plot;
Ymax<-max(max(predict(fm,list(time=xstep))),max(y))
plot(y~x,data=PKindex,type='p',main=main,ylim=c(0,Ymax*1.2),
     xlab=xaxis, ylab=yaxis,pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
points(x,y,pch=15,col="black")
lines(xstep,predict(fm,list(time=xstep)),type="l",lty=1,col="firebrick3",lwd="2")
mtext("linear plot",side=3,cex=0.88)}
###
### now with ggplot2; since v1.3.7
### the following codes work great; but cannot exist with plot() simultaneously. -YJ
###
## ggplot2## ggdata<-data.frame(time=x,conc=y)
## ggplot2## p<-ggplot(ggdata,aes(x=time,y=conc)) + scale_fill_discrete(guide=FALSE) + 
## ggplot2##    geom_point(colour="black",size=2) + geom_line(data=fittedline,aes(x=time,y=conc_fit),colour="red",size=1) + 
## ggplot2##    labs(list(title = main,x=xaxis,y=yaxis)) + theme(plot.title=element_text(size=16,face="bold"),
## ggplot2##    axis.title.x=element_text(size=16,face="bold"), axis.title.y=element_text(size=16,face="bold"))
## ggplot2## print(p)}

#Semi-log plot
plot(x,y,log="y",type='p',main=main,ylim=c(1,Ymax*1.2),### max(y)*1.1), 
     xlab=xaxis, ylab=yaxis,pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
### polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
### polygon(c(x,rev(x)),c(xx$lwr,rev(xx$upr)),border=FALSE,col="grey75")   ### use original 95% CI lines
points(x,y,pch=15,col="black")
lines(xstep,predict(fm,list(time=xstep)),type="l",lty=1,col="firebrick3",lwd="2")
mtext("semi-log plot",side=3,cex=0.88)
   
#Residual plot, time vs weighted residual
if(pick==1)
plot(x,wei,pch=16,col="blue",bty="l",xlab=xaxis,
     ylab="unweighted residual",main="unweighted residual vs. time plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
else
plot(x,wei,pch=16,col="blue",bty="l",xlab=xaxis,
     ylab="weighted residual",main="weighted residual vs. time plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
abline(h=0,lwd=2,col="black",lty=2)
  
#Residual plot, calculated concentration vs weigthed residual
if(pick==1)
plot(fitted(fm),wei,pch=16,col="blue",bty="l",xlab="Calc Cp(i)",
     ylab="unweighted residual",main="unweighted residual vs. calc. Cp plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
else
plot(fitted(fm),wei,pch=16,col="blue",bty="l",xlab="Calc Cp(i)",
     ylab="weighted residual",main="weighted residual vs. calc. Cp plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
abline(h=0,lwd=2,col="black",lty=2)
}    
