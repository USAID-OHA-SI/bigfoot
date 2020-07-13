## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  stitch together crosswalk data
## DETAIL :  append 'crosswalk' files downloaded from https://drive.google.com/drive/u/0/folders/10CIWaSqinny3RjCfsFfMPsf_Xl7GIAUj
## update:  5.28, adjust for new file structure from PSM
##          7.6 update for new file structure from PSM



#Dependancies-----------------------------------------------------
library(tidyverse)
library(vroom)
library(readxl)
library(fs)
library(glitr)
library(glamr)


#Globals----------------------------------------------------------
data_in <- "data"
data_out <- "Dataout"
x_files <- "C:/Users/Josh/Documents/GitHub/bigfoot/data/xwalk"


#read in and munge-----------------------------------------------

#list all the crosswalk files
files <- list.files(x_files, "*.xlsx", full.names = TRUE)

#function to stitch together and fix
stitch_xwalk <- function(file){
  
  df <- read_xlsx(file,
                  sheet = "Mapped Facilities v2",
                  col_types = c("text")) %>%
    mutate(country = basename(file)) %>% 
    rename_all(~tolower(.))
  
    return(df)
}

#create df
df_cross <- map_dfr(.x = files,
                    .f = ~ stitch_xwalk(.x))

 
## clean up, fix ou

xwalk <- df_cross %>% 
  mutate(country = str_remove(country, " LMIS MER Mapping v2.xlsx")) %>% 
  select(-`...7`) %>%
  rename(threshold = `similarity threshold`) %>%
  mutate_at(vars("similarity", "threshold"), as.numeric)

## prepare to join to sc_fact, keeping only vars of interest
xwalk <- xwalk %>%
  select(country, snl1, snl2, facility, sitename, country) %>% 
  mutate_all(~tolower(.))


#examine/scratch---------------------------------------------------------

# how many obs have a 0 value for similarity, ie they didn't match
xwalk %>% 
  group_by(country) %>% 
  tally(similarity == "0")
  
xwalk %>% 
  count(ou)

#how many have a non-missing orgunituid, meaning they did match
xwalk %>% 
  group_by(country) %>% 
  tally(!is.na(orgunituid))

##look for missing characters
utf8::utf8_print(unique(xwalk$facility), utf8 = FALSE)

utf8::utf8_print(unique(xwalk$sitename), utf8 = FALSE)


#read in psm-MER data-----------------------------------------------

df_mer <- read_csv("C:/Users/Josh/Documents/data/fy20_q1_v1/scm_output")

df_mer <- read_csv("../../data/fy20_q1_v1/scm_output/mer_fy20_q1_v1_site_scm.csv")






##


## scratch
## function with `fill` for threshold inputation
stitch_xwalk <- function(file){
  
  df <- read_xlsx(file,
                  sheet = "Mapped Facilities v2",
                  skip = 1,
                  col_types = c("text")) %>%
    mutate(ou = basename(file)) %>% 
    rename_all(~tolower(.)) %>% 
    fill(`similarity threshold`, .direction = "updown")
  
  return(df)
}










