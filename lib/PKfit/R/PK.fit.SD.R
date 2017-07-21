#PK model option
PK.fit.SD <- function(PKindex)
{
  OutputFilez()   ### reset all output file names when running PK.fit()
  file.menu <- c("One-Compartment PK Model", 
                 "Two-Compartment PK Model",
                 "Three-Compartment PK Model (iv bolus with 1st-ordered elim.)",
                 "Three-Compartment PK Model (iv bolus with MM elim.)",
                 "Macroconstant Exponential Functions",
                 "Go Back One Upper Level",
                 "Go Back to the Top Menu")
  
  pick <- menu(file.menu, title = "<< Selection of a PK Model - single-dosed >>")
  
  if (pick == 1){cat("\n\n");one.list.SD(PKindex)}     
  if (pick == 2){cat("\n\n");two.list.SD(PKindex)}
  if (pick == 3){cat("\n\n");fbolus3(PKindex)}
  if (pick == 4){cat("\n\n");fbolus3.mm(PKindex)}
  if (pick == 5){cat("\n\n");macro(PKindex)}
  if (pick == 6){cat("\n\n");data.manipulate(MD=FALSE)}
  if (pick == 7){cat("\n\n");run()}       
}
