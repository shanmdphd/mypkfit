### v0.2; to adapt to work with RStudio
save.plots.as.pdf<-function(plots.file,pdf_activate){
###         
### here revert between pdf() and graphic device
###
x11c<-dev.cur()  ## under Windows + RStudio, after a plot, the dev.list() incl. 'windows' and 'pdf' now
if(pdf_activate){
   dev.set(which=x11c) ## jumpt to 'windows' device first; can be 'quartz' also in Mac OSX;
   dev.copy()                           ## copy to pdf file 2nd plots to end; but device stayed at 'pdf' now
   dev.set(which=x11c) ## back from graphic device now to continue...
                }
else{
   pdf(plots.file,paper="a4")           ## v1.3.4
   ### pdf_activate=TRUE                ## set pdf_activate=TRUE from now on; need to be in the code!!
   dev.set(which=x11c) ## go to that graphics device...
   dev.copy()                           ## copy the first plot here; but device stayed at 'pdf' now
   dev.set(which=x11c) ## back back to that graphics device
    }
}
