entertitle<-function()
{
  cat("\n Enter the title of x-axis (time): \n")
  cat("(or press Enter to use default)\n\n") 
  xaxis<-readline()
  if (substr(xaxis, 1, 1) == "")  xaxis<-"time after dosing (hr)"  else xaxis<-xaxis
  cat(" x-axis -> ", xaxis,"\n\n")
  cat("\n Enter the title of y-axis(Cp): \n")
  cat("(or press Enter to use default)\n\n") 
  yaxis<-readline()
  #cat("\n\n Please Wait.  Data is Processing. \n")
  if (substr(yaxis, 1, 1) == "")  yaxis<-"drugX plasma conc. (ng/mL)"  else yaxis<-yaxis
  cat(" y-axis -> ", yaxis,"\n\n")
  #cat("\n\n Please Wait.  Data is Processing. \n")
  return(list(xaxis=xaxis,yaxis=yaxis))
}