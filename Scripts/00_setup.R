## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  structure project folders
## Modified: 9.7.21 for new si_paths

#libraries-----------------------------------------------------------------
library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(here)
library(googledrive)
library(zoo)


# set up ----------------------------------------------------------------

merdata <- glamr::si_path("path_msd")
rasdata <- glamr::si_path("path_raster")
shpdata <- glamr::si_path("path_vector")
datim   <- glamr::si_path("path_datim")  


# Set global shortcuts ----------------------------------------------------

Data <- "./Data"
Dataout <- "./Dataout"
mer_data <- "./Data/mer_data"
sc_fact <- "./Dataout"
                         
crosswalk <- "./Data/xwalk"
ppmr <- "./Data/ppmr"





