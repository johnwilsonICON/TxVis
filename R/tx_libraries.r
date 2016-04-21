#############################
#Load libraries
#############################


#install.packages("networkD3")
library(networkD3)

#install.packages("devtools")
library(devtools)
install_github("mbojan/alluvial")
library(alluvial)

#install.packages("riverplot")
library(riverplot)

#install.packages("gridExtra")
library(gridExtra)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("plyr")
library(plyr)

#install.packages("reshape")     #<-- may get loaded with plyr?
library(reshape)                 #<-- may get loaded with plyr?

#devtools::install_github("timelyportfolio/sunburstR", quiet = TRUE)
library(sunburstR)

devtools::install_github("jlopez1423/Joses-D3-Icicle-Plot", quiet = TRUE)
#Error: Does not appear to be an R package (no DESCRIPTION)
#library(Joses-D3-Icicle-Plot)

install.packages("corrplot")
library(corrplot)
