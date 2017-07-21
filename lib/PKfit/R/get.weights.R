###
### function to set weighting values for normal fitting for Subject# i
###

get.weights<-function(PKindex,pick,i){
  
  conc<-PKindex$conc[PKindex$Subject==i]

  if(pick==1) weights<- ifelse(conc==0.,1,1/conc^0)  ### equal weight
  if(pick==2) weights<- ifelse(conc==0.,1,1/conc^1)  ### 1/Cp
  if(pick==3) weights<- ifelse(conc==0.,1,1/conc^2)  ### 1/Cp^2

return(weights)
}