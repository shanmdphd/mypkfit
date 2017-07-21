macro <- function(PKindex)
{
OutputFilez() ### reset all output file names

repeat{
if(missing(PKindex)) {
readline(" Load or input the dataset first.\n  Press Enter to continue.")
data.manipulate.alpha()}
else break}

  file.menu <- c("1-Exponential Term ...(E001)", 
                 "2-Exponential Term ...(E002)", 
                 "3-Exponential Term ...(E003)",
                 "Go Back One Upper Level",
                 "Go Back to the Top Menu")
  pick <- menu(file.menu, title = "<< Macroconstants/Exponential Terms >>")
  if (pick ==1){
     cat("\n\n")
     fmacro.one(PKindex)
  }
  else if (pick == 2){
     cat("\n\n") 
     fmacro.two(PKindex)
  }
  else if (pick == 3){
     cat("\n\n") 
     fmacro.three(PKindex)
  }
  else if (pick == 4){
     cat("\n\n") 
     PK.fit(PKindex)
  }
  else if (pick == 5){
     cat("\n\n") 
     run()
  }
}
