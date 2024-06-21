# Script information ------------------------------------------------------

# generate the tabs & figs for the report for hom.27.9a 

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load packages -----------------------------------------------------------

library(tidyverse)
library(icesAdvice)
library(ss3diags)
library(r4ss)

# Load data ---------------------------------------------------------------

load("./output/retro/retrospective.RData")


# Compile results from assessment (retro0) and retros ----------------------
retro_dir <- "model/retro"
retroModels <- SSgetoutput(dirvec = file.path(retro_dir,
                                              paste("retro", 0:-5, sep = "")))
# summarize the model results
retroSummary <- SSsummarize(retroModels)

# create a vector of the ending year of the retrospectives
endyrvec <- retroSummary[["endyrs"]] + 0:-5

# retro ssb
SSplotComparisons(retroSummary,
                  endyrvec = endyrvec,
                  legendlabels = paste( 0:-5, "years"),
                  labels=c("year","SSB"),
                  subplot = c(2), # only show one plot in vignette
                  print = TRUE, # send plots to PNG file
                  plot = TRUE, # don't plot to default graphics device
                  verbose = FALSE,
                  plotdir = retro_dir
)
dev.print(jpeg,paste0("report/retro/retro_ssb.png"), width = 8, height = 7, res = 300, units = "in")
dev.off()

# retro F
SSplotComparisons(retroSummary,
                  endyrvec = endyrvec,
                  legendlabels = paste( 0:-5, "years"),
                  labels=c("year"),
                  subplot = c(8), # only show one plot in vignette
                  print = TRUE, # send plots to PNG file
                  plot = TRUE,
                  verbose = FALSE,
                  plotdir = retro_dir
)
dev.print(jpeg,paste0("report/retro/retro_f.png"), width = 8, height = 7, res = 300, units = "in")
dev.off()

# retro recruitment
SSplotComparisons(retroSummary,
                  endyrvec = endyrvec,
                  legendlabels = paste( 0:-5, "years"),
                  labels=c("year","Rec"),
                  subplot = c(10), # only show one plot in vignette
                  print = TRUE, # send plots to PNG file
                  plot = TRUE, # don't plot to default graphics device
                  verbose = FALSE,
                  plotdir = retro_dir,
                  filenameprefix="nome que queres"
)
dev.print(jpeg,paste0("report/retro/retro_rec.png"), width = 8, height = 7, res = 300, units = "in")
dev.off()

### PANEL SSB and F PLOT WITH RECENT YEARS
# library(ss3diags)
png(filename = "report/retro/retro_all.png", width = 880, height = 580, pointsize = 14)
sspar(mfrow=c(2,2),plot.cex = 0.7)
SSplotRetro(retroSummary,forecastrho = T,add=T,subplots="SSB",legendloc = "bottomright")
SSplotRetro(retroSummary,forecastrho = T,add=T,legend = F,xmin=2015)
SSplotRetro(retroSummary, subplots = "F",add=T,legendloc = "topright")
SSplotRetro(retroSummary,subplots = "F", xmin=2015,forecastrho = T,add=T,legend = F)
dev.off()


# Compute Mohn's rho ------------------------------------------------------
mohn_spawn_bio <- icesAdvice::mohn(retroSummary$SpawnBio)
mohn_fvalue <- icesAdvice::mohn(retroSummary$Fvalue)
mohn_recruits <- icesAdvice::mohn(retroSummary$recruits)

results <- data.frame(
  Variable = c("SSB", "F", "recruits"),
  Mohn_Score = c(mohn_spawn_bio, mohn_fvalue, mohn_recruits)
)

# write table
write.csv(results, "report/retro/mohn_scores.csv", row.names = FALSE)


#****************************************
# Basic Residual Diags - put in other script
#****************************************
#reference run
ss3rep = retroModels[[1]]
# Check Runs Test and Joint residuals option for mean composition data
sspar(mfrow=c(1,3),plot.cex = 0.8)
# For cpue
SSplotRunstest(ss3rep,subplots="cpue",add=T)
dev.print(jpeg,paste0("report/RunsTestResiduals_index.jpg"), width = 8, height = 7, res = 300, units = "in")
# For age
sspar(mfrow=c(1,2),plot.cex = 0.8)
SSplotRunstest(ss3rep,subplots="age",add=T)
dev.print(jpeg,paste0("report/RunsTestResiduals_age.jpg"), width = 8, height = 7, res = 300, units = "in")

# Check conflict between mean lengths
sspar(mfrow=c(1,2),plot.cex = 0.8)
SSplotJABBAres(ss3rep,subplots="age",add=T)
SSplotJABBAres(ss3rep,subplots="cpue",add=T)
dev.print(jpeg,paste0("report/JointResiduals.jpg"), width = 8, height = 3.5, res = 300, units = "in")



