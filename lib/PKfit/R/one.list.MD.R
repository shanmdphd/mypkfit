#------------Normal fitting menu----------------
one.list.MD <- function(PKindex)
{
  OutputFilez()  ### reset all output file names
  file.menu <- c("IV Route", 
                 "Non IV Route",
                 "Go Back One Upper Level",
                 "Go Back to Top Menu")
  pick <- menu(file.menu, title = "<< 1-Compartment PK Model - multiple-dosed >>")
  if (pick == 1){cat("\n\n");iv.route.MD(PKindex)}
  if (pick == 2){cat("\n\n");noniv.route.MD(PKindex)}
  if (pick == 3){cat("\n\n");PK.fit.MD(PKindex)}
  if (pick == 4){cat("\n\n"):run()}              
}
