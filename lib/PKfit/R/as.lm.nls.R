### Confidence interval for a model fitted with nls() in R
### by Viktoriia Posted on July 2, 2013	
### If you are also trying to produce the confidence interval (CI) for the model fitted with nls() 
### in R, then hopefully this piece will be helpful. Firstly, as noted on the help page for predict.nls 
### ¡§Interval argument is at present ignored¡¨, meaning it is not possible to produce the CI automatically
### as for lm(), for example. Reading a bit on the internet shows that the way around can be by using 
### as.lm.nls() from ***nls2 library***. Yet, even if one installs nls2 library and tries to call as.lm.nls(), 
### R says this function cannot be found. If you encounter this, here is a function:
### http://quantitativeconservationbiology.wordpress.com/2013/07/02/confidence-interval-for-a-model-fitted-with-nls-in-r/
###
### Using the function with predict() does produce the CI for the data points you supply in your dataset. E.g.:
### 
### [code] predCI <- predict(as.lm.nls(fittednls), interval = ¡§confidence¡¨, level = 0.95)
### 
### where fittednls is your model fitted with nls().
### 
### Do not forget to sort the results before plotting them!
### 
### However, this can be not sufficient if one wants to produce smooth CI and the original data to which the model 
### was fitted contains too few points. In that case we would have to interpolate, and approx() function can be useful:
### 
### [code] pred1 <- approx(Xvar, predCI[, 1], xout = seq(0,1,0.01))  ## fitted values
### [code] pred2 <- approx(Xvar, predCI[, 2], xout = seq(0,1,0.01))  ## lower CI
### [code] pred3 <- approx(Xvar, predCI[, 3], xout = seq(0,1,0.01))  ## upper CI
### 
### where predCI is the object obtained with the predict() function above, Xvar is the X variable in the original 
### dataset, xout is the range of values for interpolation.

### as.lm.nls <- function(object, ...) {
###  if (!inherits(object, "nls")) {
###  w <- paste("expected object of class nls but got object of class:",
###  paste(class(object), collapse = " "))
###  warning(w)
###  }
### 
###  gradient <- object$m$gradient()
###  if (is.null(colnames(gradient))) {
###  colnames(gradient) <- names(object$m$getPars())
###  }
### 
###  response.name <- if (length(formula(object)) == 2) "0" else
###  as.character(formula(object)[[2]])
### 
###  lhs <- object$m$lhs()
###  L <- data.frame(lhs, gradient)
###  names(L)[1] <- response.name
### 
###  fo <- sprintf("%s ~ %s - 1", response.name,
###  paste(colnames(gradient), collapse = "+"))
###  fo <- as.formula(fo, env = as.proto.list(L))
### 
###  do.call("lm", list(fo, offset = substitute(fitted(object))))
### 
### } 

# if object is an "nls" object then its often used like this:           
# predict(as.lm(object), ...) where ... are any predict.lm args         
                                                                        
# as.lm.nls effectively just does this:                                 
# lm(lhs ~ gradient - 1, offset = fitted(object),                       
#   list(gradient = object$m$gradient(), lhs = object$m$lhs()))         
# so most of the code is just to get the names right.                   
                                                                        
as.lm <- function(object, ...) UseMethod("as.lm")                       
                                                                        
as.lm.nls <- function(object, ...) { 
    if (!inherits(object, "nls")) {                                     
		w <- paste("expected object of class nls but got object of class:", 
			paste(class(object), collapse = " "))                             
		warning(w)                                                          
	}                                                                     
                                                                        
	gradient <- object$m$gradient()                                       
	if (is.null(colnames(gradient))) {                                    
		colnames(gradient) <- names(object$m$getPars())                     
	}                                                                     
                                                                        
	response.name <- if (length(formula(object)) == 2) "0" else           
		as.character(formula(object)[[2]])                                  
                                                                        
	lhs <- object$m$lhs()                                                 
	L <- data.frame(lhs, gradient)                                        
	names(L)[1] <- response.name                                          
                                                                        
	fo <- sprintf("%s ~ %s - 1", response.name,                           
		paste(colnames(gradient), collapse = "+"))                          
	fo <- as.formula(fo, env = as.proto.list(L)) ### ONLY with 'proto' package v0.3-10 or earlier.
	### fo <- as.formula(fo, env={L})                    ### with 'proto' package v1.0.0 or later?
                                                                        
	do.call("lm", list(fo, offset = substitute(fitted(object))))
}                                                                       