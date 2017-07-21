### Three-compartment PK model with iv bolus, single-dose, Normal fitting

options(warn=-1)
require(PKfit)
require(deSolve)
require(minpack.lm)
require(nls2)

graphics.off()
cat("\n\n")

Dose=NULL
Vm=NULL
Km=NULL
k12=NULL
k21=NULL
k13=NULL
k31=NULL
Vd=NULL

PKindex<-data.frame(Subject=c(1),time=c(5,10,15,20,30,45,60,90,120,150,180,240,360,480),    ### time
         conc=c(29.31,16.48,14.02,9.04,10.03,8.96,8,4.91,4.08,3.46,2.92,1.73,1.1,0.86))     ### drug plasma conc.
cat(" --- input dataset ---\n\n");print(PKindex, row.names=F);cat("\n")

Dose<-500   ### assign given dose here
   
   defun<- function(time, y, parms) { 
     dCp1dt <- -parms["Vm"]*y[1]/(parms["Km"]+y[1])-parms["k12"]*y[1]+parms["k21"]*y[2]-parms["k13"]*y[1]+parms["k31"]*y[3] 
     dCp2dt <-  parms["k12"]*y[1]-parms["k21"]*y[2]
     dCp3dt <-  parms["k13"]*y[1]-parms["k31"]*y[3]
     list(c(dCp1dt,dCp2dt,dCp3dt)) 
   } 

   modfun <- function(time,Vm,Km,k12,k21,k13,k31,Vd) { 
      out <- lsoda(y=c(Dose/Vd,0,0),c(0,time),defun,parms=c(Vm=Vm,Km=Km,k12=k12,k21=k21,k13=k13,k31=k31,Vd=Vd),
                  rtol=1e-06,atol=1e-06)
     #plot(out)
     out[-1,2] 
   }   

     objfun <- function(par) {
        out <- modfun(PKindex$time, par[1], par[2], par[3], par[4], par[5], par[6], par[7])
        gift <- which( PKindex$conc != 0 )
        #sum((PKindex$conc[gift]-out[gift])^2)                       ### using equal weight
        # sum((PKindex$conc[gift]-out[gift])^2/PKindex$conc[gift])    ### using weight of 1/Cp
        sum(((PKindex$conc[gift]-out[gift])/PKindex$conc[gift])^2)    ### using weight of 1/(Cp)^2
     }
     
     #opt <- optim(c(0.002443,0.041628,0.051434,0.041628,0.000512,7.237),objfun,method="Nelder-Mead",control=list(maxit=5000)) ### change initial values if necessary
     opt <- optim(c(0.1,0.1,0.2,0.2,0.3,0.3,10),objfun,method="Nelder-Mead",control=list(maxit=5000)) ### change initial values if necessary
     nameopt<-c("Vm","Km","k12","k21","k13","k31","Vd")
     outopt<-c(opt$par[1],opt$par[2],opt$par[3],opt$par[4],opt$par[5],opt$par[6],opt$par[7])
     
     cat("\n<< PK parameters obtained from Nelder-Mead Simplex algorithm >>\n\n")
     print(data.frame(Parameter=nameopt,Value=outopt), row.names=F)
     
              if(opt$par[1]<0) {opt$par[1]<-0.01}
              if(opt$par[2]<0) {opt$par[2]<-0.01}
              if(opt$par[3]<0) {opt$par[3]<-0.01}
              if(opt$par[4]<0) {opt$par[4]<-0.01}
              if(opt$par[5]<0) {opt$par[5]<-0.01}
              if(opt$par[6]<0) {opt$par[6]<-0.01}
              if(opt$par[7]<0) {opt$par[7]<-0.01}
              
     cat("\n<< Residual sum-of-squares and PK parameter values with nlsLM() >>\n\n")
     
     fm<-nlsLM(conc ~ modfun(time,Vm,Km,k12,k21,k13,k31,Vd),data=PKindex, weights=(1/conc^0), ### verify this weighting method later... -YJ
               start=list(Vm=opt$par[1],Km=opt$par[2],k12=opt$par[3],k21=opt$par[4],
               k13=opt$par[5],k31=opt$par[6],Vd=opt$par[7]),control = nls.lm.control(maxiter=500))
     ### start=list(Vm=0.1,Km=0.1,k12=0.2,k21=0.2,k13=0.3,k31=0.3,Vd=10.),control = nls.lm.control(maxiter=500))
     coef<-data.frame(coef(fm)["k31"])     

x<-PKindex$time
y<-PKindex$conc
cal<-predict(fm,list(time=x));cat("\n")

### set WSS here
#wei<-ifelse(y==0.0, 0, y-cal);cat(" weighting sheme: equal weight\n")               ### with equal weight
#wei<-ifelse(y==0.0, 0, sqrt(1/(y))*(y-cal));cat(" weighting sheme: 1/Cp\n")         ### using weight of 1/Cp
wei<-ifelse(y==0.0, 0, sqrt(1/((y)^2))*(y-cal));cat(" weighting sheme: 1/Cp^2\n")   ### using weight of 1/(Cp)^3
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
par(mfrow=c(2,2),las=1)
### plot shaded 95%CI
xx<-predict(as.lm.nls(fm),interval="confidence",level=0.95)
xx<-as.data.frame(xx)
xx$lwr<-ifelse(xx$lwr<0,0.01,xx$lwr);xx$upr<-ifelse(xx$upr<0,0.01,xx$upr)         ### cannot be zero as for semi-log plot
xy1spline <-spline(x,xx$lwr);xy1spline$y<-ifelse(xy1spline$y<0,0.01,xy1spline$y)  ### smooth the 95%CI lines
xy2spline <-spline(x,xx$upr);xy2spline$y<-ifelse(xy2spline$y<0,0.01,xy2spline$y)
### plot(out)
plot(y~x,data=PKindex,type='p',main="Drug Plasma Conc. vs. Time Curve", 
     xlab="Time", ylab="Drug Plasma Conc.",pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
### polygon(c(x,rev(x)),c(xx$lwr,rev(xx$upr)),border=FALSE,col="grey75")
polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
lines(x,predict(fm,list(time=x)),type="l",lty=1,col="firebrick3",lwd="2")
points(x,y,pch=15,col="black")
mtext("Linear",side=3,cex=0.88)
    
plot(x,y,log="y",type='p',main="Drug Plasma Conc. vs. Time Curve",
     xlab="Time", ylab="Drug Plasma Conc.",pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
### polygon(c(x,rev(x)),c(xx$lwr,rev(xx$upr)),border=FALSE,col="grey75")
polygon(c(xy1spline$x,rev(xy2spline$x)),c(xy1spline$y,rev(xy2spline$y)),border=FALSE,col="grey75")
lines(x,predict(fm,list(time=x)),type="l",lty=1,col="firebrick3",lwd="2")
points(x,y,pch=15,col="black")
lines(x,predict(fm,list(time=x)),type="l",lty=1,col="firebrick3",lwd="2")
mtext("Semi-log",side=3,cex=0.88)
     
plot(x,wei,pch=15,col="blue",bty="l",xlab="Time",ylab="Weighted Residual",main="Residual Plots",cex.lab=1,
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