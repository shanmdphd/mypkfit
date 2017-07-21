#PK model option
PK.fit.MD <- function(PKindex)
{
  OutputFilez()   ### reset all output file names when running PK.fit()
  file.menu <- c("One-Compartment PK Model", 
                 "Two-Compartment PK Model",
                 "Go Back One Upper Level",  ### no macro, of course; and no 3-compartment model for now.
                 "Go Back to the Top menu")
  
  pick <- menu(file.menu, title = "<< Selection of a PK Model - multiple-dose >>")
  
  if (pick == 1){cat("\n\n");one.list.MD(PKindex)}     
  if (pick == 2){cat("\n\n");two.list.MD(PKindex)}
  if (pick == 3){cat("\n\n");PK.fit(PKindex)}
  if (pick == 4){cat("\n\n");run()}        
}
