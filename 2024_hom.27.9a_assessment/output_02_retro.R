# Script information ------------------------------------------------------

# generate outputs from retrospective analysis for hom.27.8c9a (WGHANSA)

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# load libraries ----------------------------------------------------------

library(icesTAF)
library(icesAdvice)
library(tidyverse)
library(reshape)
library(r4ss)

# retro's directories

retro_dir <- "model/retro"
plot_dir <- "output/retro"

# set assessment year (interim year)

ass.yr <- lubridate::year(Sys.Date()) - 2 ### only for benchmark 2022 assessment for others -1


# Compile results from assessment (retro0) and retros ----------------------

dir0<- "./model/retro/retro0"
dir1 <- "./model/retro/retro-1"
dir2 <- "./model/retro/retro-2"
dir3 <- "./model/retro/retro-3"
dir4 <- "./model/retro/retro-4"
dir5 <- "./model/retro/retro-5"

retro_years<-(ass.yr-5):ass.yr

F.all<-Fbar.all<-rec.all<-bio.all<-sel.all<-c()

#read ouputs and create database for ggplots

for(i in 0:5){
  
  mydir<-get(paste0("dir",i))
  
  out.ss3<-SS_output(mydir,forecast=FALSE)
  
  selectivity<-subset(out.ss3$ageselex[out.ss3$ageselex$Fleet==1 & out.ss3$ageselex$Factor=="Asel2" & out.ss3$ageselex$Yr %in% c(out.ss3$startyr:(out.ss3$endyr)),c("Yr","0","1","2","3","4","5","6","7","8","9","10","11") ])
  
  colnames(selectivity)[1]<-"year"
  
  sel<-melt(selectivity,id.vars='year')
  
  sel.all<-rbind(sel.all,cbind(sel,retro_run=ass.yr - i))
  
  sel_bar<-rowMeans(selectivity[,3:12])   # change to fbar 1-10 columns
  
  startyr <- out.ss3$startyr
  endyr <- out.ss3$endyr
  aux <- out.ss3$derived_quants
  idx <- match(paste("F_",startyr:(endyr-1),sep=""), aux[,1])
  aux <- aux[idx, ]
  
  F.dat <- data.frame(Year=startyr:(endyr-1),
                      Value=aux$Value,
                      CV=aux$StdDev/aux$Value,
                      Lower=aux$Value-2*aux$StdDev,
                      Upper=aux$Value+2*aux$StdDev,
                      param="f apic")
  
  F.all<-rbind(F.all,cbind(F.dat,retro_run=ass.yr - i))
  
  Fbar.dat <- data.frame(Year=startyr:(endyr-1),
                         Value=aux$Value*sel_bar[-length(sel_bar)],
                         CV=aux$StdDev/aux$Value*sel_bar[-length(sel_bar)],
                         Lower=(aux$Value*sel_bar[-length(sel_bar)])-2*aux$StdDev,
                         Upper=(aux$Value*sel_bar[-length(sel_bar)])+2*aux$StdDev,
                         param="f bar")
  
  Fbar.all<-rbind(Fbar.all,cbind(Fbar.dat,retro_run=ass.yr - i))
  
  aux <- out.ss3$derived_quants
  idx <- match(paste("Recr_",startyr:endyr,sep=""), aux[,1])
  aux <- aux[idx, ] 
  rec.dat <- data.frame(Year=startyr:endyr, 
                        Value=aux$Value,
                        CV=aux$StdDev/aux$Value,
                        Lower=aux$Value-2*aux$StdDev,
                        Upper=aux$Value+2*aux$StdDev,
                        param="rec"
  )
  
  rec.all<-rbind(rec.all,cbind(rec.dat,retro_run=ass.yr - i))
  
  aux <- out.ss3$derived_quants
  idx <- match(paste("SSB_",startyr:endyr,sep=""), aux[,1])
  aux <- aux[idx, ] 
  bio.dat <- data.frame(Year=startyr:endyr, 
                        Value=aux$Value,
                        CV=aux$StdDev/aux$Value,
                        Lower=aux$Value-2*aux$StdDev,
                        Upper=aux$Value+2*aux$StdDev,
                        param="ssb"
  )
  
  bio.all<-rbind(bio.all,cbind(bio.dat,retro_run=ass.yr - i))
  
}
  
all<-rbind(bio.all,rec.all,F.all,Fbar.all)
all$peel <- as.factor(all$retro_run)
plyr::revalue(all$peel, c("0"="base"))

# Compute Mohn's rho ------------------------------------------------------

mohns_rho <- function(df){
  df <- df[,c(1,2,7)]
  df %>% tidyr::spread(retro_run, Value) %>% as.data.frame -> df
  row.names(df) <- c(df$Year)
  df <- df[,-1]
  df <- df[,c(6:1)]
  colnames(df) <- c("base","-1","-2","-3","-4","-5")
  #print(df)
  mohns <- mohn(df,details=T)
  
  return(mohns)
  
}

bio <- mohns_rho(bio.all)
rec <- mohns_rho(rec.all)
F25 <- mohns_rho(Fbar.all)

rho <- data.frame(Label=c("SSB","Recruits","Fbar1-10"),Rho=c(bio$rho,rec$rho,F25$rho))

tbl_rho <- rho%>%
  gt::gt()%>%
  gt::fmt_number(
    columns = 2,
    decimals = 3
  )

# check the Mohn's rho values
# Hurtado-Ferro et al. (2015)
# Rule of thumb for significant retrospective pattern:
# Short-lived stocks: Mohn's rho higher than 0.3 or lower than -0.22
# https://www.ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/Fisheries%20Resources%20Steering%20Group/2020/WKFORBIAS_2019.pdf?ID=36532

if (any(rho$Rho > 0.3 | rho$Rho < -0.22)){
  print("Be careful! too high Mohn's rho value(s)")
} 

# Save output objects -----------------------------------------------------

save(all,bio.all,rec.all,Fbar.all,bio,rec,F25, rho, tbl_rho, file="output/retro/retrospective.RData")

# End of script -----------------------------------------------------------

rm(list=ls())
