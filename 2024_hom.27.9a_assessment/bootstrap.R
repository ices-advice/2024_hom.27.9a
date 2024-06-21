# Script information ------------------------------------------------------

# Title: Stock assessment for southern horse mackrel (WGHANSA)
#        1) TAF bootstrap procedure to set up required data and software

# See also: https://github.com/ices-taf/doc/wiki/Creating-a-TAF-analysis 

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024

# Load libraries ----------------------------------------------------------

library(icesTAF)

# Check the working directory ---------------------------------------------

getwd()

# Create repository and R project -----------------------------------------

# Clone the github repository created by ICES secretariat
# The cloned repository is in D:/GitHUB/ices-taf/2023_hom.27.9a_assessment
# Create an R project in that folder

# Make the skeleton -------------------------------------------------------

# create initial directories and R scripts for a new TAF analysis
# 2023_hom.27.8c9a_assessment    
# ¦--bootstrap   
# ¦   --initial 
# ¦       --data
# ¦--data.R      
# ¦--model.R     
# ¦--output.R    
# °--report.R    

taf.skeleton()

# Upload initial data -----------------------------------------------------

# Include data in the folder: 2023_hom.27.8c9a_assessment\bootstrap\initial\data 
# Include all five input files needed to run Stock Synthesis
# It can be done by-hand or copying it using the following command:

# # Starter file
# file.copy(from="..../starter.ss",
#           to="./boot/initial/data/starter.ss", overwrite=T)
# 
# # Control file
# file.copy(from=".../hom.ctl",
#           to="./boot/initial/data/hom.ctl", overwrite=T)
# 
# # Data file
# file.copy(from=".../hom.dat",
#           to="./boot/initial/data/hom.dat", overwrite=T)
# 
# # Weight at age file
# file.copy(from=".../wtatage.ss",
#           to="./boot/initial/data/wtatage.ss", overwrite=T)
# 
# # Forecast file
# file.copy(from=".../forecast.ss",
#           to="./boot/initial/data/forecast.ss", overwrite=T)

# Document data and create the data.bib file ------------------------------

# use function draft.data() to create the data.bib file

# NOTE that the columns of the csv files should be separated by comma (",")

### TO RUN FIRST THIS DELETE the data.bib and software.bib

draft.data(originator="WGHANSA", 
           year=2023, 
           title="Forecast input file", 
           period="1992-2022",
           source="file",
           data.files=c("forecast.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

draft.data(originator="WGHANSA", 
           year=2023, 
           title="Starter input file", 
           period="1992-2022",
           source="file",
           data.files=c("starter.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

draft.data(originator="WGHANSA", 
           year=2023, 
           title="Control input file", 
           period="1992-2022",
           source="file",
           data.files=c("hom.ctl"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

draft.data(originator="WGHANSA", 
           year=2023, 
           title="Data input file", 
           period="1992-2022",
           source="file",
           data.files=c("hom.dat"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

draft.data(originator="WGHANSA", 
           year=2023, 
           title="Weight at age input file", 
           period="1992-2022",
           source="file",
           data.files=c("wtatage.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

# # data with stock assessment results from previous year
# draft.data(originator="WGHANSA", 
#            year=2023, 
#            title="ICES summary table from WGHANSA 2022", 
#            period="1992-2021",
#            source="file",
#            data.files=c("Table_summary.csv"), 
#            data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
#            file="boot/data.bib",
#            append=T)

# table with ss3 Model structure

draft.data(originator="WKBHMB", 
           year=2023, 
           title="Summary table of Model structure", 
           source="file",
           data.files=c("ss3ModelStructure.csv"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

# table with Data input description

draft.data(originator="WKBHMB", 
           year=2023, 
           title="Data input description table", 
           source="file",
           data.files=c("DataInputTable.csv"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

# ICES section 9 for southern horsemackerel ()

draft.data(originator="WGHANSA", 
           title="ICES TAF Word template for report automation", 
           source="file",
           data.files=c("sec_09_template.docx"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

# ICES csl file to format references 

draft.data(originator="WGHANSA", 
           title="ICES cls file", 
           source="file",
           data.files=c("ices-journal-of-marine-science.csl"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

# # horse mackerel downloaded from internet ---------------
# 
# draft.data(originator = "",
#            title = "Image horse mackerel (Sardina homchardus)",
#            period = FALSE,
#            source = "https://....jpg",
#            data.files = "horse mackerel.jpg",
#            data.scripts = NULL,
#            file = "boot/data.bib", 
#            append=T)

# # reference points
# draft.data(originator = "WKBHMB",
#            title = "Reference points for horse mackerel",
#            period = FALSE,
#            source = "file",
#            data.files = "hom_refPoints.R",
#            data.scripts = NULL,
#            file = "boot/data.bib", 
#            append=T)

# Upload software ---------------------------------------------------------

# create the folder "2023_hom.27.8c9a_assessment\boot\initial\software"

if (!dir.exists("boot/initial/software")){
  dir.create("boot/initial/software")
}

# # copy the SS3 executable file to the folder "...\boot\initial\software"
# 
# file.copy(from=".../ss3.exe",
#           to="./boot/initial/software/ss3.exe",
#           overwrite=T) # SS3 executable for windows


# Document software and create the software.bib file ----------------------

# use function draft.software to create the software.bib file

# ?draft.software

# document the SS3 model

draft.software(package=c("boot/initial/software/ss3.exe"), 
               author = "Richard D. Methot Jr., Chantel R. Wetzel, Ian G. Taylor, Kathryn L. Doering,
Elizabeth F. Gugliotti, and Kelli F. Johnson", 
               year = 2023, 
               title = "SS3 executable (for Windows)",
               version = "3.30.23",
               source = NULL, 
               file = "boot/software.bib", 
               append = FALSE)


# Process the data.bib and software.bib metafiles -------------------------

# the function taf.bootstrap() processes:
#   the data.bib file and creates/copies all the data files into the folder "bootstrap/data"
#   and 
#   the software.bib file and creates/copies the software into the folder bootstrap/software
# ?taf.bootstrap

# apply taf.bootstrap

taf.bootstrap(taf=TRUE)

# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

