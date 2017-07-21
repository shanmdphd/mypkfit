stwo.SD.all <- function()
{
OutputFilez()  ### reset all output file names
  cat("************************\n")
  cat(" Extravas: Extravascular\n")
  cat(" 1st-Ord: First-Ordered \n")
  cat(" Abs: Absorption        \n")
  cat(" w/o: without           \n")
  cat(" Tlag: Lag Time         \n")
  cat("************************\n\n")
  cat("\n")
  file.menu <- c("IV-Bolus ...................... (SD201)", 
                 "IV-Infusion ................... (SD202)",
                 "Extravas & 1st-Ord Abs w/o Tlag (SD203)",
                 "Go Back One Upper Level",
                 "Go Back to Top Menu")
  pick <- menu(file.menu, title = "<< 2-Compartment PK Model - single-dosed >>")
  if (pick ==1){
     cat("\n\n")
     sbolus2(MD=FALSE)
  }
  else if (pick == 2){
     cat("\n\n") 
     sinfu2(MD=FALSE)
  }
  else if (pick == 3){
     cat("\n\n") 
     sfirst2(MD=FALSE)
  }
  else if (pick == 4){
     cat("\n\n") 
     PK.sim.SD()
  } 
  else if (pick == 5){
     cat("\n\n") 
     run()
  } 
}
