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
library(ICPIutilities)


#Globals----------------------------------------------------------
data_in <- "data"
data_out <- "Dataout"
x_files <- "C:/Users/Josh/Documents/GitHub/bigfoot/data/xwalk"


#read in and munge-----------------------------------------------

#list all the crosswalk files
files <- list.files(x_files, "*.xlsx", full.names = TRUE)

#function to stitch together and fix
stitch_xwalk <- function(file){
  
  df <- readxl::read_xlsx(file,
                  sheet = "Mapped Facilities v2",
                  col_types = c("text")) %>%
    dplyr::mutate(country = basename(file)) %>% 
    dplyr::rename_all(~tolower(.))
  
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

#duplicate check

xwalk <- xwalk %>% 
  group_by(country, facility) %>% 
  mutate(n = n(),
         dup_flag = row_number()) %>% 
  ungroup()

xwalk %>%
  filter(n>1) %>% 
  arrange(country, facility, orgunituid) %>%
  view()

# xwalk %>%
#   filter(n > 1) %>%
#   arrange(country, facility, orgunituid) %>%
#   write_csv(file.path(data_out, "scfact_xwalk_duplicates_v2.csv"))

## prepare to join to sc_fact, keeping only vars of interest
## 7/16 update, drop duplicates
xwalk <- xwalk %>%
  filter(n == 1) %>% 
  select(country, facility, orgunituid) %>% 
  mutate_at(vars(country, facility), ~tolower(.))

#clean up workspace-----------------------------------------------------
rm(df_cross)
rm(stitch_xwalk)
rm(x_files)
rm(dup_check)

#examine/scratch---------------------------------------------------------

# how many obs have a 0 value for similarity, ie they didn't match

dup_check <- xwalk %>% 
  group_by_all() %>% 
  mutate(n = n(),
         dup_flag = row_number()) %>% 
  ungroup()

dup_check %>% filter(n > 1) %>% arrange(country, facility, orgunituid) %>% View()

df %>% 
  filter(country == "Botswana",
         snl1 %in% c("gantsi", "charleshill")) %>% view()

    

xwalk %>% 
  group_by(country) %>% 
  tally(similarity == "0")
  
xwalk %>% 
  count(ou)

xwalk %>% 
  group_by(country) %>% 
  distinct(snl1) %>%
  arrange(country, snl1) %>% 
  prinf()

df_long_dedup %>% 
  group_by(country) %>% 
  distinct(snl2) %>% prinf()

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










