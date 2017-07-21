### this function is used to switch graphic device (GD) if using RStudio
### from 'RStudioGD' to x11 or windows or quartz; i.e., using an
### external GD, instead of the default GD ('RStudioGD'); the plot windows
### will be popup.
### frequent used exmaples of device:
#########################################
### dev.off(which=dev.list()["pdf"])      --> to close the pdf file; current GD will switch to the other active GD
### dev.set(which=dev.list()["windows"])  --> to switch the current GD to windows GD
### dev.copy()                            --> always copy a graphic from the current GD to the other.
### dev.off()                             --> close the current GD only
### pdf(...)                              --> after this statement, the current GD is a 'pdf'
#########################################
setGD<- function(){

if(.Platform$GUI=="Rgui") 
   grDevices::windows.options(record=TRUE,restoreConsole=TRUE) ### set Rgui to record plots
   
if(.Platform$GUI=="RStudio"){
      o = tolower(Sys.info()["sysname"])
      a = switch(o,
                 "darwin"  = "quartz",
                 "linux"   = "x11",
                 "windows" = "windows")
      options("device" = a)
} ### else options("device"="RStudioGD")  ### remarked this; otherwise, it would defintely req. RStudio.
}