#------------Simulation menu----------------
sone.iv.route.MD <- function()
{
OutputFilez()  ### reset all output file names
  cat("***************************************\n")
  cat(" MM elim: Michaelis-Menten Elimination \n")
  cat("***************************************\n\n")
  cat("\n")
  file.menu <- c("IV-Bolus & the 1st-ordered elim .. (MD101)", 
                 "IV-Bolus & MM elim ............... (MD102)", 
                 "IV-Infusion with 1st-ordered elim. (MD103)",
                 "IV-Infusion with MM elim ......... (MD104)",
                 "Go Back One Upper Level",
                 "Go Back to Top Menu")
  pick <- menu(file.menu, title = "<< IV Route - multiple-dosed >>")
  if (pick ==1 ){
     cat("\n\n")
     sbolus1(MMe=FALSE,MD=TRUE)
  }
  else if (pick == 2){
     cat("\n\n") 
     sbolus1(MMe=TRUE,MD=TRUE)
  }
   else if (pick == 3){
     cat("\n\n") 
     sinfu1(MMe=FALSE,MD=TRUE)
  }
   else if (pick == 4){
     cat("\n\n") 
     sinfu1(MMe=TRUE,MD=TRUE)
  }
  else if (pick == 5){
     cat("\n\n") 
     PK.sim.MD()
  }
  else if (pick == 6){
     cat("\n\n") 
     run()
  }
}
