###
### use a amazing function here ':='; WORKS GREAT!! 
### Ref. link: https://stackoverflow.com/questions/1826519/function-returning-more-than-one-value
###            https://code.google.com/p/miscell/source/browse/rvalues/rvalues.r
### still don't know how to write an Rd for this function yet...
###
### ':=' <- function(lhs, rhs) {
###   frame <- parent.frame()
###   lhs <- as.list(substitute(lhs))
###   if (length(lhs) > 1)
###     lhs <- lhs[-1]
###   if (length(lhs) == 1) {
###     do.call(`=`, list(lhs[[1]], rhs), envir=frame)
###     return(invisible(NULL)) 
###   }
###   if (is.function(rhs) || is(rhs, 'formula'))
###     rhs <- list(rhs)
###   if (length(lhs) > length(rhs))
###     rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
###   for (i in 1:length(lhs))
###     do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
###   return(invisible(NULL)) 
### }

para_err<-function(sim_types){
pick=NULL
type=NULL
cat("\n\n")
if(sim_types=="sim"){
   file.menu <- c("No Error",
                  "Error = Normal Error", 
                  "Error = Uniform Error",
                  "Error = True Value*Normal Error",
                  "Error = True Value*Uniform Error")
    pick <- menu(file.menu, title = "<< Error Types >>")
    type<-switch(pick,
                 "No Error",
                 "Error = Normal Error", 
                 "Error = Uniform Error",
                 "Error = True Value*Normal Error",
                 "Error = True Value*Uniform Error")}
else{
   file.menu <- c("Error = Normal Error",            
                  "Error = Uniform Error",
                  "Error = True Value*Normal Error",
                  "Error = True Value*Uniform Error")
    pick <- menu(file.menu, title = "<< Error Types >>")
    type<-switch(pick, 
                 "Error = Normal Error",            
                 "Error = Uniform Error",
                 "Error = True Value*Normal Error",
                 "Error = True Value*Uniform Error")}
 return(list(pick=pick,type=type))
}
