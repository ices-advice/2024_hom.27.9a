# Script information ------------------------------------------------------

# Title: Stock assessment and short term forecast 
#        4) Prepare tables and figures for the report

# Before running the script in folder output we have: 
#         XXXX
# After running the script in folder report we have: 
#         XXXX

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024


# Load libraries ----------------------------------------------------------

library(icesTAF)

# Working directory and folders -------------------------------------------

#check working directory

getwd()

# create model folder and subfolders using the function mkdir from icesTAF

mkdir("report")
mkdir("report/retro")
mkdir("report/STF")
mkdir("report/tables")

# Create report figs and tabs ---------------------------------------------

source("report_01_run.R")
source("report_02_retro.R")
# source("report_03_stf.R") # Deterministic forecast not used

# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

