# Script information ------------------------------------------------------

# generate the tabs & figs for the report for hom.27.9a 

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2022/11/18

# Load packages -----------------------------------------------------------


library(icesTAF)

# STF table

file.copy(from="output/STF/hom9a_stf_advice.csv",
          to="report/STF/hom9a_stf_advice.csv", 
          overwrite=T)

# End of script -----------------------------------------------------------

rm(list=ls())

