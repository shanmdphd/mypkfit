description_version<-function(){
cat("\n")
cat("-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-\n\n")
cat("   8888888b.  888    d8P   .d888 d8b 888     \n")
cat("   888   Y88b 888   d8P   d88P   Y8P 888     \n")
cat("   888    888 888  d8P    888        888     \n")
cat("   888   d88P 888d88K     888888 888 888888  \n")
cat("   8888888P   8888888b    888    888 888     \n")
cat("   888        888  Y88b   888    888 888     \n")
cat("   888        888   Y88b  888    888 Y88b.   \n")
cat("   888        888    Y88b 888    888   Y888  \n\n")
cat("-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-\n\n")
cat(" This report was generated using PKfit v1.3.8\n")
cat(" on:-",date(),"\n")
username<-Sys.info()[['user']]
if(username=="datawalker"||username=="ptpc_tawian"||username=="dindongkas"||username=="dindongtas") username<-"the developer - YJL"
if(.Platform$OS.type=="windows") osname_version<- win.version()   ### only for R3.2.3 or above
 else osname_version<-c(paste(Sys.info()[['sysname']],"-",Sys.info()[['version']],"-",Sys.info()[['machine']]))
cat(paste(" ",R.Version()[['version.string']],"\n (nickname: ",R.version$nickname,")\n",sep=""))  ### since v1.3.7
cat("   user id:",username,"\n")
cat(" system OS:",osname_version,"\n\n")
cat(" PKfit is developed by Chun-ying Lee & Yung-jin Lee.\n")
cat(" contact: Yung-jin Lee <mobilepk at gmail.com> \n\n")
cat(" PKfit is under license of GPL-2|GPL-3.\n\n")
cat(" citation:\n")
cat("  Lee, Chun-ying and Lee, Yung-jin (2017). PKfit:\n")
cat("  Data Analysis Tool for Pharmacokinetics. v1.3.8,\n")
cat("  <URL: http://pkpd.kmu.edu.tw/pkfit>.\n")
cat(" Bug Report: mobilepk at gmail.com\n")
cat("..................................................\n\n")
}