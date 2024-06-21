# Script information ------------------------------------------------------

# run TAF analysis for hom.27.9a (WGHANSA)

# Authors: Hugo Mendes (hmendes@ipma.pt) and Laura Wise(lwise@ipma.pt) 

# Date: 2024


# Load packages -----------------------------------------------------------

library(icesTAF)
library(rmarkdown)

# clean the TAF directories (all except bootstrap/initial):
#clean()

# Run the TAF analysis ----------------------------------------------------

# run step by step

sourceTAF("bootstrap")
sourceTAF("data")
sourceTAF("model")
sourceTAF("output")
sourceTAF("report")

# run in a go: utilities.R, data.R, model.R, output.R, and report.R.
# note that bootstrap.R is not run

#sourceAll()


# End of script -----------------------------------------------------------


