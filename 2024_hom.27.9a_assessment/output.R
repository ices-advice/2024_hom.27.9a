# Script information ------------------------------------------------------

# Title: Stock assessment and short term forecast for hom.27.9a (WGHANSA)
#        4) Extract results of interest, write TAF output tables

# Before running the script in folder data we have: 
#         folders with the assessment run and the retros
# After running the script in folder model we have: 
#         folders with the corresponding outputs (including reference points)

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load libraries ----------------------------------------------------------

library(icesTAF)

# Working directory and folders -------------------------------------------

# check working directory

getwd()

# create output folder and sub-folders using the function mkdir from icesTAF

mkdir("output")
mkdir("output/run")
mkdir("output/retro")
mkdir("output/toFLR")
mkdir("output/STF")

# Outputs from stock assessment -------------------------------------------

source("output_01_run.R")

# Outputs from retro ------------------------------------------------------

source("output_02_retro.R")

# Outputs: create FLR objects ---------------------------------------------

 # source("output_03_toFLR.R") # Deterministic forecast not used

# Outputs from BRP (EqSim) -----------------------------------

# Run if needed (takes a long time!)

# source("output_04_brp.R")

# Outputs: create STF objects ---------------------------------------------

source("output_04_STF.R")


# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

rm(list = ls())
