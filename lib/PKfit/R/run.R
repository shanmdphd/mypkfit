#PKfit main menu
run <- function(first.run=TRUE) 
{
## Need to clean up the requirements.
  options(warn=-1)   ### since v1.3.7
  options(width=80)  ### for output width
  if(first.run==TRUE) setGD()     ### if running on RStudio, set the GD as generic one (windows/x11/quartz); since v1.3.7
  
### if(nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))){  ### OR can be coded as
### if(.Platform$GUI=="RStudio") {cat("\n\n GUI is RStudio.\n")}
### if(.Platform$GUI=="Rgui")    {cat("\n\n GUI is Rgui (R console).\n")}

cat("\n\n")
file.menu <- c("Normal Fitting","Simulation","Quit")
pick <- menu(file.menu, title = "<< PKfit:- Top menu >>")

if (pick == 1){cat("\n\n");PK.fit()}   ### after adding multiple-dose fitting since v1.3.0
if (pick == 2){cat("\n\n");PK.sim()}
if (pick == 3){cat("\n Thank for using PKfit. Bye now.\n\n")}
}
