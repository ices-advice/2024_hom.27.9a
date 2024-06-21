# Script information ------------------------------------------------------

# run stock assessment for southern horse mackerel
# settings: spaly

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load packages -----------------------------------------------------------

library(r4ss)

# Get input files --------------------------------------------------------

cp("boot/data/hom.dat", "model/run")
cp("boot/data/hom.ctl", "model/run")
cp("boot/data/forecast.ss", "model/run")
cp("boot/data/starter.ss", "model/run")
cp("boot/data/wtatage.ss", "model/run")

# Get model executable ----------------------------------------------------
cp("boot/software/ss3.exe", "model/run")

# Save main directory
wd <- getwd()

# Run SS from r (need to change directory to run SSs)
setwd("model/run")

system("./ss3.exe")

setwd(wd)
 
#setwd(rprojroot::find_rstudio_root_file())

# End of script -----------------------------------------------------------

rm(list=ls())

