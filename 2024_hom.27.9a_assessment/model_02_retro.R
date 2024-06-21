# Script information ------------------------------------------------------

# run retrospective analysis for hom.27.9a 

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load packages -----------------------------------------------------------

library(r4ss)
library(icesTAF)
library(lubridate)

# Run retrospective analysis ----------------------------------------------

# copy files to the retro directory --

retro_dir <- "./model/retro"

r4ss::copy_SS_inputs(dir.old = "./model/run", dir.new = retro_dir,overwrite = TRUE) # copy over the stock synthesis model

# copy ss3 to the retro directory --

# Get model executable --
icesTAF::cp("boot/software/ss3.exe", "model/retro")

# do retros from the current year to -5 --
# this creates and runs all the retros

retro(dir = retro_dir, oldsubdir = "", newsubdir = "", 
           years = 0:-5,exe = "ss3.exe")

# Set ass.yr
ass.yr <- year(Sys.Date()) - 2 ### only for benchmark 2022 assessment for wghansa is -1


# End of script -----------------------------------------------------------

rm(list=ls())

