noniv.route.SD <- function(PKindex)
{
OutputFilez()  ### reset all output file names
  cat("***************************************\n")
  cat(" Extravas: Extravascular               \n")
  cat(" 1st-Ord: First-Ordered                \n")
  cat(" Zero-Ord: Zero-Ordered                \n")
  cat(" Abs: Absorption; elim: elimination    \n")
  cat(" w: with; w/o: without; Tlag: Lag Time \n")
  cat(" MM elim.: Michaelis-Menten elimination\n")
  cat("***************************************\n\n")
  cat("\n")

  file.menu <- c("Extravas & 1-Ord Abs w Tlag ............. (SD105)",
                 "Extravas & 1-Ord Abs w/o Tlag ........... (SD106)",
                 "Extravas, 1-Ord Abs & MM elim w Tlag .... (SD107)",
                 "Extravas, 1-Ord Abs & MM elim w/o Tlag .. (SD108)",
                 "Extravas, Zero-Ord Abs w/o Tlag ......... (SD109)",
                 "Extravas, Zero-Ord Abs & MM elim w/o Tlag (SD110)",
                 "Go Back One Upper Level",
                 "Go Back to Top Menu")
  pick <- menu(file.menu, title = "<< Non IV Route - single-dosed >>")
  if (pick == 1){
     cat("\n\n") 
     ffirst.lag(PKindex, MD=FALSE)
  }
  else if (pick == 2){
     cat("\n\n") 
     ffirst.nolag(PKindex, MD=FALSE)
  }
  else if (pick == 3){
     cat("\n\n") 
     ffirst.lagm.SD(PKindex, MD=FALSE)
  }
  else if (pick == 4){
     cat("\n\n") 
     ffirst.nolagm.SD(PKindex, MD=FALSE)
  }
  else if (pick == 5){
     cat("\n\n") 
     fzero.nolag(PKindex, MD=FALSE)
  }
  else if (pick == 6){
     cat("\n\n") 
     fzero.nolagm.SD(PKindex, MD=FALSE)
  }
  else if (pick == 7){
     cat("\n\n") 
     one.list.SD(PKindex)
  }
  else if (pick == 8){
     cat("\n\n") 
     run()
  }
}
