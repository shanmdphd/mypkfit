iv.route.SD <- function(PKindex)
{
OutputFilez()  ### reset all output file names
  cat("***************************************\n")
  cat(" MM elim: Michaelis-Menten elimination \n")
  cat("***************************************\n\n")
  cat("\n")
  
  file.menu <- c("IV-Bolus & 1st-ordered elim .. (SD101)", 
                 "IV-Bolus & MM elim ........... (SD102)", 
                 "IV-Infusion & 1st-ordered elim (SD103)",
                 "IV-Infusion & MM elim ........ (SD104)",
                 "Go Back One Upper Level",
                 "Go Back to Top Menu")
  pick <- menu(file.menu, title = "<< IV Route - single-dosed >>")
  if (pick ==1){
     cat("\n\n")
     fbolus1(PKindex, MD=FALSE)
  }
  else if (pick == 2){
     cat("\n\n") 
     fbolus.mm.SD(PKindex, MD=FALSE)
  }
  else if (pick == 3){
     cat("\n\n") 
     finfu1(PKindex, MD=FALSE)
  }
  else if (pick == 4){
     cat("\n\n") 
     finfu.mm.SD(PKindex, MD=FALSE)
  }
  else if (pick == 5){
     cat("\n\n") 
     one.list.SD(PKindex)
  }
  else if (pick == 6){
     cat("\n\n") 
     run()
  }
}
