PK.sim <- function()
{
  OutputFilez() ### reset all output file names when running PK.sim()
  file.menu <- c("Single-Dose ............... (SD)", 
                 "Multiple-Dose ............. (MD)",
                 "Macroconstants/Exponential Terms",
                 "Go Back to the Top Menu")
  pick <- menu(file.menu, title = "<< Selection of Dosing Type >>")
  if (pick == 1){
     cat("\n\n")  
     PK.sim.SD()
  }
  else if (pick == 2){
     cat("\n\n")
     PK.sim.MD()
  }
  else if (pick == 3){
     cat("\n\n")
     smacro()
  }
  else if (pick == 4){
     cat("\n\n")
     run()
  }
}