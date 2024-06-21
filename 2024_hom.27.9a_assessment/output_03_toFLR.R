# Script information ------------------------------------------------------

# create FLStock from stock assessment output for hom.27.8c9a (WGHANSA)

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load libraries --------------------------------------------------------------

library(ss3om)
library(tidyverse)

# Load FLR object from folder containing the output of the SS3 run ------------

dir <- "model/run"

ass.yr <- lubridate::year(Sys.Date()) - 2 ### only for benchmark 2022 assessment for others -1

# To create an FLStock object from a model run, we can call function readFLSss3

hom.stock <- ss3om::readFLSss3(dir, name = "hom.27.9a", desc = paste0('Advice for ', ass.yr+1))

out <- readOutputss3(dir, repfile = "Report.sso", compfile = "CompReport.sso")

# Min, Max Fbar

hom.stock@range[['minfbar']] <- 1
hom.stock@range[['maxfbar']] <- 10

# Harvest should take in consideration Fbar

selectivity <- out$ageselex%>%
  dplyr::filter(Fleet == 1 & Factor == "Asel2" & Yr %in% out$startyr:out$endyr)%>%
  dplyr::select("Yr",matches("[0-9]+"))

f.apical <- out$derived_quants%>%
  dplyr::filter(grepl("F_\\d", Label))%>%
  dplyr::select(Label,Value)%>%
  tidyr::separate(Label, into = c("Variable","Yr"),sep = "_",remove = TRUE,extra="drop")

hom.stock@range[['minfbar']] <- 1
hom.stock@range[['maxfbar']] <- 10    ### or 9 to change BAU 

# fbar ( 1-10)
harvest <- f.apical$Value*selectivity[,-1]%>%
  dplyr::mutate(refF = rowMeans(dplyr::select(., "1","2","3","4","5","6","7","8","9","10")))
harvest$Year <- f.apical$Yr

harvest=FLQuant(unname(as.matrix(t(dplyr::select(harvest,-c('refF',"Year"))))),quant="age",units="f")
dimnames(harvest)[[2]]=as.character(out$startyr:(out$endyr))
dimnames(harvest)[[1]]=as.character(min(out$agebins):max(out$agebins))

harvest(hom.stock) <- harvest

units(m.spwn(hom.stock)) <- "f"

# Discards.wt should be zero
discards.wt(hom.stock) <- discards.n(hom.stock)

# save the objects created ------------------------------------------------

save(hom.stock, file="output/toFLR/homStock.RData")

# End of script -----------------------------------------------------------

rm(list = ls())


