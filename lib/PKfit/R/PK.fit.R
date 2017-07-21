#PK model option
PK.fit <- function(PKindex)
{
  OutputFilez()   ### reset all output file names when running PK.fit()
  file.menu <- c("Single-Dose .............. (SD)", 
                 "Multiple-Dose ............ (MD)",
                 "Macroconstant/Exponential Terms",
                 "Go Back the Top Menu")
  
  pick <- menu(file.menu, title = "<< Selection of Dosing Type >>")
  
  if (pick == 1){cat("\n\n");data.manipulate(MD=FALSE)}     
  if (pick == 2){cat("\n\n");data.manipulate(MD=TRUE)}
  if (pick == 3){cat("\n\n");data.manipulate.alpha()}
  if (pick == 4){cat("\n\n");run()}        
}
