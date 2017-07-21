#------------Normal fitting menu----------------
one.list.SD <- function(PKindex)
{
  OutputFilez()  ### reset all output file names
  file.menu <- c("IV Route", 
                 "Non IV Route",
                 "Go Back One Upper Level",
                 "Go Back to Top Menu")
  pick <- menu(file.menu, title = "<< 1-Compartment PK Model - single-dosed >>")
  if (pick == 1){cat("\n\n");iv.route.SD(PKindex)}
  if (pick == 2){cat("\n\n");noniv.route.SD(PKindex)}
  if (pick == 3){cat("\n\n");PK.fit.SD(PKindex)}
  if (pick == 4){cat("\n\n");run()}           
}
