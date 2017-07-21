### One-compartment PK model with iv bolus, single-dose, Normal fitting

options(warn=-1)
require(PKfit)
require(deSolve)
require(minpack.lm)
graphics.off()
cat("\n\n")

Dose=NULL
kel=NULL
Vd=NULL

PKindex<-data.frame(Subject=c(1),time=c(5,10,15,20,30,45,60,90,120,150,180,240,360,480),    ### time
         conc=c(29.31,16.48,14.02,9.04,10.03,8.96,8,4.91,4.08,3.46,2.92,1.73,1.1,0.86))     ### drug plasma conc.
cat("\n\n --- input dataset ---\n\n");print(PKindex, row.names=F);cat("\n")

Dose<-500   ### assign given dose here

defun<- function(time, y, parms) { 
      dCpdt <- -parms["kel"] * y[1] 
      list(dCpdt) 
} 
    
modfun <- function(time,kel, Vd) {  
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(kel=kel,Vd=Vd),
                   rtol=1e-6,atol=1e-6) 
      out[-1,2] 
}

objfun <- function(par) {
        out <- modfun(PKindex$time, par[1], par[2])
        gift <- which( PKindex$conc != 0 )
        #sum((PKindex$conc[gift]-out[gift])^2)                       ### using equal weight
        sum((PKindex$conc[gift]-out[gift])^2/PKindex$conc[gift])    ### using weight of 1/Cp
        #sum(((PKindex$conc[gift]-out[gift])/PKindex$conc[gift])^2)  ### using weight of 1/(Cp)^2
}        

opt<-optim(c(0.01,10),objfun,method="Nelder-Mead")  ### change initial values if necessary
nameopt<-c("kel","Vd")
outopt<-c(opt$par[1],opt$par[2])
    
fm<-nlsLM(conc ~ modfun(time, kel, Vd), data=PKindex,
        start=list(kel=opt$par[1],Vd=opt$par[2]))###,trace=TRUE,
        ### nls.control(maxiter=5000,tol=1e-06,minFactor=1/1024/1024),algorithm = "port",lower=c(0,0))
coef<-data.frame(coef(fm)["kel"])

x<-PKindex$time
y<-PKindex$conc
cal<-predict(fm,list(time=x));cat("\n")

### set WSS here
#wei<-ifelse(y==0.0, 0, y-cal);cat("weighting scheme: equal weight\n")                   ### with equal weight
wei<-ifelse(y==0.0, 0, sqrt(1/(y))*(y-cal));cat("weighting scheme: 1/Cp\n")             ### using weight of 1/Cp
#wei<-ifelse(yy[j]==0.0, 0, sqrt(1/((y)^2))*(y-cal));cat("weighting scheme: 1/Cp^2\n")   ### using weight of 1/(Cp)^2
###
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
AUC<-c(NA,add1$auc[-1])
AUMC<-c(NA,add1$aumc[-1])
output<-data.frame(x,y,cal,wei,AUC,AUMC)
colnames(output)<-list("Time","Observed","Calculated","Wt. Residuals","AUC","AUMC")

auc.infinity<-y[length(y)]/coef[1,1]
auc<-AUC[length(y)]+auc.infinity

aumc.infinity<-(x[length(x)]*y[length(y)])/coef[1,1]+y[length(y)]/((coef[1,1])^2)
aumc<-AUMC[length(y)]+aumc.infinity

### windows(record=TRUE)
dev.new()
par(mfrow=c(2,2), ask = FALSE)

plot(y~x,data=PKindex,type='p',main="Drug Plasma Conc. vs. Time Curve", 
     xlab="Time", ylab="Drug Plasma Conc.",pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
lines(x,predict(fm,list(time=x)),type="l",lty=1,
      col="firebrick3",lwd="2")
mtext("Linear",side=3,cex=0.88)
    
plot(x,y,log="y",type='p',main="Drug Plasma Conc. vs. Time Curve",
     xlab="Time", ylab="Drug Plasma Conc.",pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
lines(x,predict(fm,list(time=x)),type="l",lty=1,
      col="firebrick3",lwd="2")
mtext("Semi-log",side=3,cex=0.88)
     
plot(x,wei,pch=15,col="blue",bty="l",xlab="Time",
     ylab="Weighted Residual",main="Residual Plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
abline(h=0,lwd=2,col="black",lty=2)
    
plot(cal,wei,pch=15,col="blue",bty="l",xlab="Calc Cp(i)",
     ylab="Weighted Residual",main="Residual Plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
abline(h=0,lwd=2,col="black",lty=2)
cat("\n")
cat("--- Output --- \n\n")
print(output, row.names=F)
aicllsbc(fm)
cat("\n\n")

       