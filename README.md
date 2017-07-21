#How to install/upgrade PKfit manually? 
PKfit website: [http://pkpd.kmu.edu.tw/pkfit](http://pkpd.kmu.edu.tw/pkfit)

1. **the first time to install PKfit**  If you never install PKfit before, please download the file [**preinst.r** ](https://sourceforge.net/projects/yjlee-r-packages/files/) from the top file list. Place the file under the working directory (use **getwd()** under R console/terminal to find out where it is.) For linux PC OS, please make sure that you have installed all required development tools for R packages. And then download the bundle file (`PKfit_x.x.x.zip` for Windows OS, `PKfit_x.x.x.tgz` for iMac OS X, or `tar.gz` for linux PC OS). To use **preinst.r**, you need to have administrator privilege. This is very important for Windows 10, iMac OS X and Linux PC. Then open R, type **source("preinst.r")** and select which package you want to install. The file **preinst.r** will first update all currently installed packages, and then install all required packages for the selected package. If the platform is Windows OS, it will continue to install the package with a local zip file. Otherwise, it will stop after installation of all required packages. Users of iMac OS X and Linux PC need to install the package **manually** (sorry about that). For iMac OS X and Linux PC users, please go to the next step  for more details.          


2. **upgrade from the previous version (PKfit v1.2.4 or above)**
   
     - **install from the binary file** Just download `PKfit_x.x.x.zip` (for Windows OS) or `PKfit_x.x.x.tgz` (for iMac OS X) from this site. Under R console (for Windows OS; iMac OS X users should choose install from a binary file), click ***Packages*** from the top menu and then click ***Install packages from local zip files...*** Find and select `PKfit_x.x.x.zip` (for Windows OS) or `PKfit_x.x.x.tgz` (for iMac OS X) that you just download to install it.   
     
     - **install from the source file** ***(This method should work for all platforms)*** The source file can be downloaded from the directory of `src` of download site. The source file should be like `PKfit_x.x.x.tar.gz`. Please always download the most updated version. Windows OS and iMac OS X may require the development tool which can be downloaded from CRAN mirror sites. Linux can be installed directly. Under a terminal, type `R CMD INSTALL PKfit_x.x.x.tar.gz` (for Windows OS 7/8.x; Windows 10 users, press "Windows key + x" together and click                                    "Command Prompt (Admin)"); or `sudo R CMD INSTALL PKfit_x.x.x.tar.gz` (Linux and iMac OS X; then enter your 'su' password)

3. **for different platforms**	
    
    - **MS-WINDOWS** (installation from the binary file)

		   1. Download 'PKfit_x.x.x.zip' from this site to your computer first.
		   2. Under R console (Rgui.exe), click ***Packages*** from the top menu and then click ***Install packages from local zip files...***. Finally, select `PKfit_x.x.x.zip` to install it.
		   3. If you like to go back to previous version, please go to the directory of 'src' to download the source file (PKfit_x.x.x.tar.gz). And type `R CMD INSTALL PKfit_x.x.x.tar.gz` (you may need to install development tool to install PKfit from the source file).    
           
    - **iMac OS X** (installation from the binary file; if not work, please install from the source files; see above)          
	
		   1. Download "PKfit_x.x.x.tgz' to your home folder; 
		   2. Open Terminal and type `sudo R CMD INSTALL PKfit_x.x.x.tgz` and enter your 'su' password.    
		   
    - **Linux-PC** (installation from the binary file)     
	
		   1. Download 'PKfit_x.x.x_R_i686-pc-linux-gnu.tar.gz' (for x86 linux) or 'PKfit_x.x.x_R_x86_64-pc-linux-gnu.tar.gz' (for x64 linux) first;
		   2. Open Terminal and type `sudo R CMD INSTALL PKfit_x.x.x_R_i686-pc-linux-gnu.tar.gz` (for x86 linux) or `sudo R CMD INSTALL PKfit_x.x.x_R_x86_64-pc-linux-gnu.tar.gz` (for x64 linux)

	That's all. Thanks for using PKfit. --YJ
