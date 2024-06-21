# Script information ------------------------------------------------------

# Read assessment data (after the bootstrap procedure) and write TAF data tables

# Before running the script in folder ./bootstrap/initial/data we have: 
#         forecast.ss 
#         hom.ctl
#         hom.dat
#         starter.ss
#         wtatage.ss
#         and other files needed later
# After running the script in folder ./data we have:
#         inputData.RData with r4ss input object and TAF .csv data tables:
#         natmort, catage, catch, natage_idx_ibts, ntotal_idx_ibts,
#         survey_DEPM, survey_CPUE, waca, west, fecundity, maturity

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load libraries ----------------------------------------------------------

library(icesTAF)
library(r4ss)

# Working directory and folders -------------------------------------------

# check working directory

getwd()

# create data folder using the function mkdir in icesTAF

mkdir("data")

# Read data ---------------------------------------------------------------

# read input files
inputs <- r4ss::SS_read(dir = "boot/data", verbose = TRUE)


# Prepare TAF tables ------------------------------------------------------

# natural maturity table
natmort <- inputs$ctl$natM

# historical numbers at age in the catch table
catage <- subset(inputs$dat$agecomp,FltSvy==1 & Yr>=(inputs$dat$styr) & Yr<(inputs$dat$endyr),c("Yr","a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"))

# catch in tonnes
catch <- subset(inputs$dat$catch, year>=(inputs$dat$styr), c('year','catch'))

# numbers at age in the IBTS survey 
natage_idx_ibts <- subset(inputs$dat$agecomp,FltSvy==2 & Yr%in% inputs$dat$styr:inputs$dat$endyr,c("Yr","a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"))

# total numbers in the IBTS survey 
ntotal_idx_ibts <- subset(inputs$dat$CPUE,index==2,c("year","obs"))

# ssb estimated in the DEPM survey (spawning biomass)
survey_DEPM <- subset(inputs$dat$CPUE,index==4,c("year","obs"))

# CPUE estimated in the commercial fleet
survey_CPUE <- subset(inputs$dat$CPUE,index==3,c("year","obs")) 

# weight at age in the catch
waca <- subset(inputs$wtatage, Fleet=="1" & Yr %in% inputs$dat$styr:inputs$dat$endyr,c("Yr","0","1","2","3","4","5","6","7","8","9","10","11"))

# weight at age in the stock (same as in catch)
west <- subset(inputs$wtatage, Fleet=="3" & Yr %in% inputs$dat$styr:inputs$dat$endyr,c("Yr","0","1","2","3","4","5","6","7","8","9","10","11"))

# fecundity
fecundity <- subset(inputs$wtatage, Fleet=="-2" & Yr %in% inputs$dat$styr:inputs$dat$endyr,c("Yr","0","1","2","3","4","5","6","7","8","9","10","11"))

# maturity
maturity <- fecundity/west


# Write TAF tables in data folder -----------------------------------------

write.taf(list(natmort=natmort, catage = catage, catch = catch, 
                natage_idx_ibts = natage_idx_ibts, ntotal_idx_ibts = ntotal_idx_ibts,
               survey_DEPM = survey_DEPM, survey_CPUE = survey_CPUE,
               waca = waca, west = west, fecundity = fecundity, maturity = maturity),dir="./data")

# Save data in RData file  -----------------------------------------

save.image("./data/inputData.RData")

# Script info -------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

rm(list=ls())
