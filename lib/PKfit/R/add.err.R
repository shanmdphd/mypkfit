###
add.err<-function(integratedCp){
### first to convert Cp to matrix and then add rnorm
### integratedCp<-as.matrix(integratedCp)+as.matrix(runif(length(integratedCp),
###               min=-0.1*integratedCp,max=0.1*integratedCp))   ### if using runif()...
### integratedCp<-jitter(integratedCp)   ### if using jitter()
###
integratedCp<-as.matrix(integratedCp)+as.matrix(integratedCp)*
  as.matrix(runif(length(integratedCp),min=-0.15,max=0.15))   ### adding '<= +/- 15% errors of calc. conc.'
  ### as.matrix(rnorm(length(integratedCp),mean=0,sd=0.1))    ### adding about '<= +/- 20~30% errors of calc. conc.'
integratedCp<-as.numeric(integratedCp)                        ### convert Cp back to numeric
### integratedCp<-ifelse(integratedCp<=0,0,integratedCp)      ### not req.; set Cp as zero if it is a negative
return(integratedCp)
}