smacro <- function()
{
OutputFilez()  ### reset all output file names
  file.menu <- c("1-Exponential Term ...(E001)", 
                 "2-Exponential Term ...(E002)", 
                 "3-Exponential Term ...(E003)",
                 "Go Back One Upper Level",
                 "Go Back to the Top Menu")
  pick <- menu(file.menu, title = "<< Macroconstants/Exponential Terms >>")
  if (pick ==1){
     cat("\n\n")
     smacro.one()
  }
  else if (pick == 2){
     cat("\n\n") 
     smacro.two()
  }
  else if (pick == 3){
     cat("\n\n") 
     smacro.three()
  }
  else if (pick == 4){
     cat("\n\n") 
     PK.sim()
  }
  else if (pick == 5){
     cat("\n\n") 
     run()
  }
}
