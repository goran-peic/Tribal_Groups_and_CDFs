###############################################################################
## Setup script loads required packages, sets up directories, and themes     ##
###############################################################################

library(foreign)
try(library(sandwich), install.packages("sandwich"))
try(library(pscl), install.packages("pscl"))
try(library(MASS), install.packages("MASS"))
try(library(lmtest), install.packages("lmtest"))
try(library(foreign), install.packages("foreign"))
try(library(ggplot2), install.packages("ggplot2"))
try(library(extrafont), install.packages("extrafont"))
font_install("fontcm", prompt=F)
loadfonts()

dataDir <- paste(homeDir, "Data", sep="\\")
outputDir <- paste(homeDir, "Figures_Tables", sep="\\")

spartan.theme <- theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(size=.4)
)
					
#################################### Setup ####################################