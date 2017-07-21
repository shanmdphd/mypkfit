iv.route.MD <- function(PKindex)
{
OutputFilez()  ### reset all output file names
  cat("***************************************\n")
  cat(" MM elim: Michaelis-Menten elimination \n")
  cat("***************************************\n\n")
  cat("\n")
  
  file.menu <- c("IV-Bolus & 1st-ordered elim .. (MD101)", 
                 "IV-Bolus & MM elim ........... (MD102)", 
                 "IV-Infusion & 1st-ordered elim (MD103)",
                 "IV-Infusion & MM elim ........ (MD104)",
                 "Go Back One Upper Level",
                 "Go Back to Top Menu")
  pick <- menu(file.menu, title = "<< IV Route - multiple-dosed >>")
  if (pick ==1){
     cat("\n\n")
     fbolus1(PKindex, MD=TRUE)
  }
  else if (pick == 2){
     cat("\n\n") 
     fbolus.mm.MD(PKindex, MD=TRUE)
  }
  else if (pick == 3){
     cat("\n\n") 
     finfu1(PKindex, MD=TRUE)
  }
  else if (pick == 4){
     cat("\n\n") 
     finfu.mm.MD(PKindex, MD=TRUE)
  }
  else if (pick == 5){
     cat("\n\n") 
     one.list.MD(PKindex)
  }
  else if (pick == 6){
     cat("\n\n") 
     run()
  }
}
