## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  structure project folders

#libraries-----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(vroom)
library(ICPIutilities)
library(glamr)
library(glitr)
library(googledrive)
library(googlesheets4)
library(here)

# set up ----------------------------------------------------------------

#glamr::si_setup()
#glamr::folder_setup()


# Set global shortcuts ----------------------------------------------------

Data <- "./Data"
Dataout <- "./Dataout"
mer_data <- "./Data/mer_data"
sc_fact <- "./Data/sc_fact"
crosswalk <- "./Data/xwalk"



