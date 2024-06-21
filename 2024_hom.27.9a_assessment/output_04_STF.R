#----------------------------------------------------------------#
# Short-term deterministic forecast with FLR packages
# FLash and FLAssess (versions Feb 2017) only run with R_32 bits
# Requires FLStock object
# #----------------------------------------------------------------#

# Authors: Hugo Mendes (hmendes@ipma.pt)

# Date: 2024

#!first time FLR packages installation use this:
#source("http://flr-project.org/R/instFLR.R")
rm(list=ls())
library(FLAssess) # install.packages("FLAssess", repos="http://flr-project.org/R")
library(FLash) # install.packages("FLash", repos="http://flr-project.org/R")

# load FLStock object ------------------------------------------------------

load("./output/toFLR/homStock.RData")
hom<-hom.stock
ls()
slotNames(hom)
catch(hom); fbar(hom); rec(hom); ssb(hom)
year <- range(hom)[["minyear"]]:range(hom)[["maxyear"]]

as.numeric(range(hom)[("maxyear")])
range(hom)
#----------------------------------------------------
#           User defined settings  
#-----------------------------------------------------
# Nb projection years (usually 3: final year assessment + 2)
# hom9a Stock Annex: Fsq is F last assessment year (until implementation of MP)
final_year <- as.numeric(range(hom)[("maxyear")]+3)
start_year <- as.numeric(range(hom)[("maxyear")])+1
proj_years <- as.numeric(range(hom)[("maxyear")]+2):final_year
nb_proj_years <- length(proj_years)
# stock.n(hom_proj); harvest(hom_proj)
avg.years=3 # years to average Wt and selectivity

#Reference Points
Blim <- 201000
Bpa <- 279000
Flim <- 0.158
Fp05 <- 0.146
Fpa <- 0.146
Fmsy <- 0.115
MSYBtrigger <- 279000

#### Set Recruitment scenario to deal with uncertainty with survey and selectivity issues ####
# scenario1a (CHOOSEN FOR ADVICE)
s1a <- exp(mean(log(rec(hom[,ac("1992":c(start_year-1))]))))  # survey/med - 92-17 all-yr (full uncertainty not accept 2018)

## fix selected recruitment/scenario
geomean_rec <- s1a
# geomean_rec <- s1b

hom_sr <- list(model="mean", params=FLPar(a = geomean_rec))
hom_sr 

# Replace R2022 by geomean (R in last assessment year is usually highly uncertain no survey info age0)
hom_proj <- stf(hom, nyears = length(start_year), wts.nyears=avg.years, fbar.nyears=avg.years) ## no need we project interim year but if changes in method of recruitment keep  
N0_replace <- geomean_rec
## replace recruitment in stock numbers 
stock.n(hom_proj)[1, as.character(start_year-1)] <- N0_replace

#----------------------------------------------------
#              run determinist projections  
#-----------------------------------------------------
# Set up Fsq (Fbar (1-10)) and Fmult scenarios
f_years <- 1 # last assessment year
fsq <- exp(mean(log(fbar(hom)[,(length(year) - f_years + 1):length(year)])))

## find Fmean multipliers for BRP advice catch options
mult <- c(Fmsy,Fpa,Flim)/fsq #MP=0.11,Fmsy&Fpa=0.15,Flim=0.19

## change Fmultiplier to get all the necessary Fmsy,Fpa,Fmp###########
fmult <- c(0,1,1.2,1.6,2,mult) # f multipliers for F BRP

# ## Other catch options F SSB2024=Bpa/Bmsy ...... Bpa=181, Blim=103
# fmult <- seq(80,150, 1) # big ranges 1 
# fmult <- seq(102.3,102.4, 0.001) # refine ranges 0.01
# fmult <- seq(102.602,102.603,0.0001) # refine ranges 0.001

fbar_fmult <- fsq*fmult

#project initial pop to interim year
ctrl <- fwdControl(data.frame(year = start_year, quantity = "f", val = fsq))
hom_interim <- fwd(hom_proj, ctrl = ctrl, sr = hom_sr)

#nb_scenarios <- length(fbar_fmult)
fbar_scenarios <- matrix(NA, ncol=nb_proj_years, nrow=length(fbar_fmult))
dimnames(fbar_scenarios) <- list(scenario = 1:length(fbar_fmult), year = proj_years)
fbar_scenarios[,] <- fbar_fmult
proj_det <- list()

# Loop over Fmult scenarios -- ##
hom_interim <- stf(hom_interim, nyears = nb_proj_years, wts.nyears=avg.years, fbar.nyears=avg.years) ## if needed change average years 
for (scenario in 1:nrow(fbar_scenarios))
{ ctrl_target <- data.frame(year = proj_years,
                            quantity = "f",
                            val = fbar_scenarios[scenario,],maxF=4)
ctrl_f <- fwdControl(ctrl_target,maxF=4)
hom_proj_fwd <- fwd(hom_interim, ctrl = ctrl_f, sr = hom_sr,maxF=4)
proj_det[[scenario]] <- hom_proj_fwd
}

slotNames(proj_det[[1]])  #Fmult=0 & Fsq=0

#----------------------------------------------------
#              Prepare STF table  keep assessment years outputs in table for checks
#-----------------------------------------------------
proj_tab <- data.frame(nb_scen=1:length(fbar_fmult), fmult=fmult, fbar=fbar_scenarios[,1])

ncol_start <- ncol(hom_proj_fwd)-3
ncol_end <- ncol(hom_proj_fwd)
rec_out <- data.frame(matrix(vector(), ncol=length(ncol_start:ncol_end)))
ssb_out <- data.frame(matrix(vector(), ncol=length(ncol_start:ncol_end)))
catch_out <-data.frame(matrix(vector(), ncol=length(ncol_start:ncol_end)))

for (i in 1:length(fmult)) { 
  rec <- rec(proj_det[[i]][, ncol_start:ncol_end])
  rec_out <- rbind(rec_out, rec)
  ssb <- ssb(proj_det[[i]][, ncol_start:ncol_end])
  ssb_out <- rbind(ssb_out,ssb)
  catch <- catch(proj_det[[i]][, ncol_start:ncol_end])
  catch_out <- rbind(catch_out, catch) 
}

names(rec_out) <- c(paste('rec', ac(start_year-1)), paste('rec',ac(start_year)), paste('rec',ac(start_year+1)), paste('rec',ac(start_year+2)))
names(ssb_out) <- c(paste('ssb', ac(start_year-1)), paste('ssb',ac(start_year)), paste('ssb',ac(start_year+1)), paste('ssb',ac(start_year+2)))
names(catch_out) <- c(paste('c', ac(start_year-1)), paste('c',ac(start_year)), paste('c',ac(start_year+1)), paste('c',ac(start_year+2)))

proj_tab <- cbind(proj_tab, rec_out, ssb_out, catch_out)
proj_tab


write.csv(proj_tab, 'output/STF/hom9a_stf_advice.csv')