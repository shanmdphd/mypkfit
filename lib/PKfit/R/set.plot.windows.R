set.plot.windows<- function(){    ### since v1.3.7
if(.Platform$OS.type=="windows") {
          graphics.off();windows(record=TRUE,restoreConsole=TRUE)
          par(mfrow=c(2,1))}
if(.Platform$OS.type=="unix" ) {  ### incl. linux & Mac OSX
          graphics.off();dev.new()
          par(mfrow=c(2,1))}
}
