#Estimate model fitting: AIC, Log likelihood, SBC
aicllsbc <- function(fm)
{   

  #Summary the results of nls
  print(summary(fm), row.names=F)
  ModelSelect<-data.frame(Model_Select_Index=c("AIC","Log Likelihood","SBC/BIC"),
                                      Values=c(AIC(fm),logLik(fm),BIC(fm)))
  print(ModelSelect, row.names=F);cat("\n")
  
  cat("<< Variance-Covariance Matrix >>\n")
  print(vcov(fm), row.names=F)
  
  ### cat("\n\n");print(confint(fm),row.names=F);cat("\n")   ### not working
  ### cat("\n<< weights >>\n")  ### for debugging purpose. remark this since v1.2.6. -YJ
  ### print(weights(fm), row.names=F)
}  
