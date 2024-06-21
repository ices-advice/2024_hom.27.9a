# Script information ------------------------------------------------------

# generate the tabs & figs for the report for hom.27.8c9a 

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)

# working directory

wd <- getwd()

# Assessment year
ass.yr <- lubridate::year(Sys.Date()) - 1 ### only for benchmark 2022 assessment for others -1

# directory with output files

dir0 <- file.path("model/run")
spaly <- r4ss::SS_output(dir = dir0,forecast=FALSE)
# read in ss3 files 

inputs <- r4ss::SS_read(dir = dir0, verbose = TRUE)

# Figures -----------------------------------------------------------

# Age composition

fleets <- c("1" = "Commercial catches","2" = "IBTS survey")

inputs$dat$agecomp %>%
  pivot_longer(cols = starts_with("a", ignore.case = FALSE), names_to = "Age", values_to = "Number") %>%
  dplyr::filter(FltSvy %in% c(1, 2)) %>%
  mutate(Age = factor(Age, levels = paste0("a", 0:11))) %>% # order ages
  ggplot() +
  geom_bar(aes(x = Yr, y = Number / sum(Number), fill = Age, group = Yr), position = "fill", stat = "identity", colour = NA) +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "Year", y = "Proportion", title = "Age composition", fill = "Age") + 
  facet_grid(FltSvy ~ ., labeller = labeller(FltSvy = fleets))
ggsave("AgeComposition.png",path = "./report")

# Model fit to IBTS Survey

file.copy(from="output/run/plots/index2_cpuefit_IBTS.png",
          to="report/index2_cpuefit_IBTS_survey.png", 
          overwrite=T)

# Model fit to DEPM Survey

file.copy(from="output/run/plots/index2_cpuefit_DEPM.png",
          to="report/index2_cpuefit_DEPM.png", 
          overwrite=T)

# Model fit to CPUE commercial

file.copy(from="output/run/plots/index2_cpuefit_CPUE_trawl.png",
          to="report/index2_cpuefit_CPUE_trawl.png", 
          overwrite=T)

  file.copy(from="output/run/plots/index5_logcpuefit_CPUE_trawl.png",
          to="report/index5_logcpuefit_CPUE_trawl.png", 
          overwrite=T)
  
# Age composition fit
  
  file.copy(from="output/run/plots/comp_agefit__multi-fleet_comparison.png",
            to="report/comp_agefit__multi-fleet_comparison.png", 
            overwrite=T)
  
  file.copy(from="output/run/plots/comp_agefit__aggregated_across_time.png",
            to="report/comp_agefit__aggregated_across_time.png", 
            overwrite=T)
  
  file.copy(from="output/run/plots/comp_agefit_flt2mkt0.png",
            to="report/comp_agefit_flt2mkt0.png", 
            overwrite=T)
  
  file.copy(from="output/run/plots/comp_agefit_residsflt2mkt0_page2.png",
            to="report/comp_agefit_residsflt2mkt0_page2.png", 
            overwrite=T)
  
  file.copy(from="output/run/plots/comp_agefit_residsflt1mkt0_page2.png",
            to="report/comp_agefit_residsflt1mkt0_page2.png", 
            overwrite=T)
  
  
  file.copy(from="output/run/plots/comp_agefit_flt1mkt0_page2.png",
            to="report/comp_agefit_flt1mkt0_page2.png", 
            overwrite=T)
  
  file.copy(from="output/run/plots/comp_agefit_flt2mkt0_page2.png",
            to="report/comp_agefit_flt2mkt0_page2.png", 
            overwrite=T)
  
  file.copy(from="output/run/plots/comp_agefit_flt2mkt0_page1.png",
            to="report/comp_agefit_flt2mkt0_page1.png", 
            overwrite=T)
  
# Selectivity blocks
  
  file.copy(from="output/run/plots/sel11_timevary_surf_flt1sex1.png",
            to="report/sel11_timevary_surf_flt1sex1.png", 
            overwrite=T)
  
  file.copy(from="output/run/plots/sel14_age_flt2sex1.png",
            to="report/sel14_age_flt2sex1.png", 
            overwrite=T)
  
# SRR 
  
  
  file.copy(from="output/run/plots/SR_curve2.png",
            to="report/SR_curve2.png", 
            overwrite=T)


# Comparison between last year and this year's assessment -----------------


# read last year's assessments / keep this for WGHANSA 2024 / in benchmark writes the same model
  dirLastYear <- "boot/initial/data/SS3_LastYearRun"  ##
  
  years<-(ass.yr-1):ass.yr
  F.all<-Fbar.all<-rec.all<-bio.all<-sel.all<-c()
  
  for(i in years){
    
    if(i == ass.yr){
      mydir <- file.path(dir0)} else {
        mydir <- file.path(dirLastYear)}
    
    
    out.ss3<-r4ss::SS_output(dir = mydir,forecast=FALSE)
    
    selectivity<-subset(out.ss3$ageselex[out.ss3$ageselex$Fleet==1 & out.ss3$ageselex$Factor=="Asel2" & out.ss3$ageselex$Yr %in% c(out.ss3$startyr:(out.ss3$endyr)),c("Yr","0","1","2","3","4","5","6","7","8","9","10","11") ])
    
    colnames(selectivity)[1]<-"year"
    
    sel<-reshape::melt(selectivity,id.vars='year')
    
    sel.all<-rbind(sel.all,cbind(sel,run=i))
    
    sel_bar<-rowMeans(selectivity[,3:12])
    
    startyr <- out.ss3$startyr
    endyr <- out.ss3$endyr
    aux <- out.ss3$derived_quants
    idx <- match(paste("F_",startyr:(endyr),sep=""), aux[,1])
    aux <- aux[idx, ]
    
    F.dat <- data.frame(Year=startyr:(endyr),
                        Value=aux$Value,
                        CV=aux$StdDev/aux$Value,
                        Lower=aux$Value-2*aux$StdDev,
                        Upper=aux$Value+2*aux$StdDev,
                        param="f apic")
    
    F.all<-rbind(F.all,cbind(F.dat,run=i))
    
    Fbar.dat <- data.frame(Year=startyr:(endyr),
                           Value=aux$Value*sel_bar,
                           CV=aux$StdDev/aux$Value*sel_bar,
                           Lower=(aux$Value*sel_bar)-2*aux$StdDev,
                           Upper=(aux$Value*sel_bar)+2*aux$StdDev,
                           param="fbar(2-10)")
    
    Fbar.all<-rbind(Fbar.all,cbind(Fbar.dat,run=i))
    
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
    
    rec.all<-rbind(rec.all,cbind(rec.dat,run=i))
    
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
    
    bio.all<-rbind(bio.all,cbind(bio.dat,run=i))
    
  # rm(aux,bio.dat,F.dat,Fbar.dat,rec.dat,sel,selectivity,endyr,i,idx,mydir,out.ss3,sel_bar,startyr)
  }
  
  
  # plots
  ggplot(bio.all, aes(x = Year,y=Value/1000000)) +
    geom_pointrange(aes(ymin = Lower/1000000, ymax = Upper/1000000,
                        shape = as.factor(run), linetype = as.factor(run)),
                    position = position_dodge(width=.5) ) +
    scale_shape_manual(values = c(1, 16)) +
    scale_linetype_manual(values = c(2, 1)) +
    scale_y_continuous(limits = c(0, NA)) +  
    theme(text = element_text(size = 14),
          plot.background =	element_rect(colour = NA, fill = NA),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = c(0.9,0.875)) +
    labs(x="Year", y="SSB, million tonnes",shape="Assessment", linetype="Assessment")
  ggsave("report/biomass.png",width=12,height = 8)
  
  # FISHING MORTALITY
  
    ggplot(Fbar.all, aes(x = Year, y = Value)) +
    geom_pointrange(aes(ymin = Lower, ymax = Upper,
                        shape = as.factor(run), linetype = as.factor(run)),
                    position = position_dodge(width = .5)) +
    scale_shape_manual(values = c(1, 16)) +
    scale_linetype_manual(values = c(2, 1)) +
    scale_y_continuous(limits = c(0, NA)) +  # This sets the y-axis to start at 0
    theme(text = element_text(size = 14),
          plot.background = element_rect(colour = NA, fill = NA),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = c(0.9, 0.875)) +
    labs(x = "Year", y = latex2exp::TeX('$Fbar_{1-10}$'), shape = "Assessment", linetype = "Assessment")
  ggsave("report/fbar.png",width=12,height = 8)
  
  # RECRUITMENT
  
  ggplot(rec.all, aes(x = Year,y=Value/1000000)) +
    geom_pointrange(aes(ymin = Lower/1000000, ymax = Upper/1000000,
                        shape = as.factor(run), linetype = as.factor(run)),
                    position = position_dodge(width=.5)) +
    scale_shape_manual(values = c(1, 16)) +
    scale_linetype_manual(values = c(2, 1)) +
    scale_y_continuous(limits = c(0, NA)) +  
    theme(text = element_text(size = 14),
          plot.background =	element_rect(colour = NA, fill = NA),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = c(0.9,0.875)) +
    labs(x="Year",y="Age 0 recruits billions",shape="Assessment", linetype="Assessment")
  ggsave("report/Recruits.png",width=12,height = 8)
  
# Tables  -------------------------------------------------------

  # Natural mortality

tbl_natmort <- spaly$Natural_Mortality[1,13:24]%>%
    dplyr::mutate(label = "M") %>%
  #  dplyr::select(8, 1:12) %>%
    gt::gt()%>%
    gt::cols_label('11' = "11+", "label" = "")%>%
    gt::tab_spanner(
      label = "Age",
      columns = 2:13,
    ) %>%
    gt::fmt_passthrough(columns = 1, escape = F)
  gt::gtsave(tbl_natmort, "./report/tables/tbl_natmort.rtf")  

# Parameters table
tbl_params <- spaly$parameters%>%
  dplyr::filter(!is.na(Active_Cnt))%>%
  dplyr::select(Label,Value,Parm_StDev,Phase,Min,Max,Init)%>%
  gt::gt()%>%
  gt::fmt_number(
    columns = 2:3,
    decimals = 3
  )
gt::gtsave(tbl_params, "./report/tables/tbl_params.rtf")

# F-at-age (Fbar 1-10 years) table

selectivity <- spaly$ageselex%>%
  dplyr::filter(Fleet == 1 & Factor == "Asel2" & Yr %in% spaly$startyr:spaly$endyr)%>%
  dplyr::select("Yr",matches("[0-9]+"))

f.apical <- spaly$derived_quants%>%
  dplyr::filter(grepl("F_\\d", Label))%>%
  dplyr::select(Label,Value)%>%
  tidyr::separate(Label, into = c("Variable","Yr"),sep = "_",remove = TRUE,extra="drop")

f.table2rep <- f.apical$Value*selectivity[,-1]%>%
  dplyr::mutate(refF = rowMeans(dplyr::select(.,"1","2","3","4","5","6","7","8","9","10"))) 
f.table2rep$Year <- f.apical$Yr

tbl_fatage <- f.table2rep%>%
  dplyr::rename_with(~paste0("Age ", .), grep("[0-9]+", names(.)))%>%
  dplyr::select(Year, refF, starts_with("Age"))%>%
  gt::gt()%>%
  gt::fmt_number(
    columns = -Year,
    decimals = 3
  )%>%
  gt::tab_footnote(
    footnote = gt::md("refF is equal to Fbar(1-10), the reference fishing mortality, corresponding to the average F of ages 1 to 10"),
    locations = gt::cells_column_labels(
      columns = refF))
gt::gtsave(tbl_fatage, "./report/tables/tbl_fatage.rtf")

# N-at-age table

tbl_natage <- spaly$natage%>%
  dplyr::filter(Era=="TIME" & `Beg/Mid`=="B") %>%
  dplyr::select("Yr",matches("[0-9]+"))%>%
  dplyr::rename(Year = Yr)%>%
  gt::gt()%>%
  gt::fmt_number(
    columns = -Year,
    decimals = 0,
    use_seps = FALSE
  )%>%
  gt::cols_label('11' = "11+")%>%
  gt::tab_spanner(
    label = "Age",
    columns = 2:13,
  ) 
gt::gtsave(tbl_natage, "./report/tables/tbl_natage.rtf")

# Catch-at-age

tbl_catage <- inputs$dat$agecomp%>%
  dplyr::filter(FltSvy == 1)%>%
  dplyr::select(Yr,starts_with("a",ignore.case = F))%>%
  dplyr::rename_with(~ gsub("a", "", .x))%>%
  gt::gt()%>%
  gt::cols_label(Yr = "Year", '11' = "11+")%>%
  gt::tab_spanner(
    label = "Age",
    columns = 2:13 ) 

gt::gtsave(tbl_catage , "./report/tables/tbl_catage.rtf")

# Weights-at-age in the catch

tbl_weca <- spaly$wtatage%>%
  dplyr::filter(Fleet == 1 & Yr<spaly$endyr)%>%
  dplyr::select("Yr",matches("[0-9]+"))%>%
  gt::gt()%>%
  gt::cols_label(Yr = "Year", '11' = "11+")%>%
  gt::tab_spanner(
    label = "Age",
    columns = 2:13 ) 
gt::gtsave(tbl_weca, "./report/tables/tbl_weca.rtf")

# Weights-at-age in the stock

tbl_west <- spaly$wtatage%>%
  dplyr::filter(Fleet == 3)%>%
  dplyr::select("Yr",matches("[0-9]+"))%>%
  gt::gt()%>%
  gt::cols_label(Yr = "Year", '11' = "11+")%>%
  gt::tab_spanner(
    label = "Age",
    columns = 2:13 ) 
gt::gtsave(tbl_west, "./report/tables/tbl_west.rtf")

# Maturity-at-age in the stock

mat <- spaly$wtatage%>%
  dplyr::filter(Fleet == -2)%>%
  dplyr::select(matches("[0-9]+"))/spaly$wtatage%>%
  dplyr::filter(Fleet == 3)%>%
  dplyr::select(matches("[0-9]+"))

mat$Year <- spaly$startyr:spaly$endyr
mat$`0` <- 0

tbl_mat <- mat%>%
  dplyr::select(Year,matches("[0-9]+"))%>%
  gt::gt()%>%
  gt::fmt_number(
    columns = 3:13,
    decimals = 3
  )%>%
  gt::cols_label('11' = "11+")%>%
  gt::tab_spanner(
    label = "Age",
    columns = 2:13
  )
gt::gtsave(tbl_mat, "./report/tables/tbl_mat.rtf")

#  hom in 9a: standard graph table. 
# biomass and landings in t, recruits in thousands of individuals, F in year -1
rec <- subset(rec.all, run == ass.yr)%>%select('Year','Lower','Value','Upper')
ssb <- subset(bio.all, run == ass.yr)%>% ## this is SSB
  select('Year','Lower','Value','Upper')
fbar <- subset(Fbar.all, run == ass.yr)%>%select('Year','Lower','Value','Upper')
tabxx <- merge(rec,ssb,by='Year', all.x=T)

catches_ass <- spaly$catch[spaly$catch$Yr>=spaly$startyr,c("Yr","Obs")]
colnames(catches_ass) <- c("Year","catch input")

tabxx <- merge(tabxx,catches_ass,by="Year",all.x = TRUE)

tabxx <- merge(tabxx, fbar, by='Year', all.x=T)

names(tabxx) <- c("Year","R_low","Recruits","R_high","ssb_low","ssb","ssb_high",
                  "catch input","F_low","Fbar","F_high")

tabxx <- round(tabxx,3)
#Clip to paste in ICES template:
clipr::write_clip(tabxx,col.names=F)

tabxx%>%
  gt::gt()%>%gt::gtsave("./report/tables/tab_sag.rtf")

# 
# #  hom in 9a: Summary table of the WGHANSA ass.yr assessment. 
# # CVs are presented for SSB, recruitment and Apical F (maximum F-at-age by year); 
# # biomass and landings in t, recruits in thousands of individuals, F in year -1
# 

rec <- subset(rec.all, run == ass.yr)%>%select(Year,Recruits = Value,'CV recruits' = CV)

ssb <- subset(bio.all, run == ass.yr)%>%
       select('Year','Value','CV')%>%
       left_join(spaly$timeseries %>% select(Yr, SSB = SpawnBio),
                   by = c(Year = "Yr"))%>%
        select(Year,'ssb' = Value, SSB, 'CV SSB' = CV)

fbar <- subset(Fbar.all, run == ass.yr)%>%
  select(Year,'F(1-10)' = Value,'CV F' = CV)%>%
  left_join(f.apical %>% select(FApical = Value, Yr) %>% mutate_at("Yr", as.integer),
            by = c(Year = "Yr"))%>%
  select(Year,'F(1-10)','CV F',FApical)

tabSummary <- merge(ssb,rec, by ='Year', all.x=T)
tabSummary <- merge(tabSummary, fbar, by='Year', all.x=T)

tabSummary <- merge(tabSummary,catches_ass,by="Year",all.x = TRUE)

tabSummary <- round(tabSummary,3) 

tabSummary%>%
  gt::gt()%>%
  gt::gtsave("./report/tables/tab_report.rtf")

# Save tables in RData file

save(tbl_catage,tbl_fatage,tbl_mat,tbl_natage,tbl_natmort,tbl_params,
     tabxx,tabSummary,tbl_weca,tbl_west,file="./report/tables/tablesReport.RData") # tbl_ref.pts


# End of script -----------------------------------------------------------

rm(list=ls())

