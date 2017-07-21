### 
### checking if parameters are entered correcly or not.
### x is a data.frame!
###
check.para<-function(x){
repeat{
 check.this.column<-x
 check.this.column<-check.this.column[,-1]
 check.this.column<-as.data.frame(check.this.column)
 has.neg <- apply(check.this.column,1,function(row) any(row <= 0))
 if(sum(has.neg)>0){
   cat("\n")
   cat("****************************************** \n")
   cat(" Any parameter value here can not be zero  \n")
   cat(" or less than zero.                        \n")
   cat(" Press Enter to continue.                  \n")
   cat("*******************************************\n\n")
   cat("  Check the row #: ", which(has.neg),"\n\n")
   readline()
   x<-edit(x)} 
 else break
 }
return(x)
}