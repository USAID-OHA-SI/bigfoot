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
library(here)


#Globals----------------------------------------------------------
data_in <- "data"
data_out <- "Dataout"
#x_files <- "C:/Users/Josh/Documents/GitHub/bigfoot/data/xwalk"
x_files <- here(data_in, "xwalk")
mer <- "C:/Users/Josh/Documents/data/fy20_q3_v1/site_level_for_lmis"

#read in and munge-----------------------------------------------

# ## update for google drive
# folder <- "1DYnNzypGeUFpc23DkHdVBazvDVN9c5Dk"
# 
# drive_files <- drive_ls(as_id(folder)) %>% pull(name)

# 
# #list all the crosswalk files
files <- list.files(x_files, "*.xlsx", full.names = TRUE)


#function to stitch together and fix
stitch_xwalk <- function(file){
  
  sheetname <- readxl::excel_sheets(file) %>% 
    pluck(grep("Mapped", .))
  
  df <- readxl::read_xlsx(file,
                  sheet = sheetname,
                  col_types = c("text")) %>%
    dplyr::mutate(country = basename(file)) %>% 
    dplyr::rename_all(~tolower(.))
  
    return(df)
}

#create df
df_cross <- purrr::map_dfr(.x = files,
                    .f = ~ stitch_xwalk(.x))

 
## clean up, fix ou

xwalk <- df_cross %>% 
  mutate(country = str_remove(country, "\\ .*")) %>% 
  mutate_at(vars("similarity"), as.numeric) %>% 
  select(country, facility, orgunituid)



## prepare to join to sc_fact, keeping only vars of interest
## 7/16 update, drop duplicates
xwalk <- xwalk %>%
  select(country, facility, orgunituid) %>% 
  mutate_at(vars(country, facility), ~tolower(.))

#clean up workspace-----------------------------------------------------
rm(df_cross)
rm(stitch_xwalk)
rm(x_files)

# #examine/scratch---------------------------------------------------------
# 
# # how many obs have a 0 value for similarity, ie they didn't match
# 
# dup_check <- xwalk %>% 
#   group_by_all() %>% 
#   mutate(n = n(),
#          dup_flag = row_number()) %>% 
#   ungroup()
# 
# dup_check %>% filter(n > 1) %>% arrange(country, facility, orgunituid) %>% View()
# 
# df %>% 
#   filter(country == "Botswana",
#          snl1 %in% c("gantsi", "charleshill")) %>% view()
# 
#     
# 
# xwalk %>% 
#   group_by(country) %>% 
#   tally(similarity == "0")
#   
# xwalk %>% 
#   count(ou)
# 
# xwalk %>% 
#   group_by(country) %>% 
#   distinct(snl1) %>%
#   arrange(country, snl1) %>% 
#   prinf()
# 
# df_long_dedup %>% 
#   group_by(country) %>% 
#   distinct(snl2) %>% prinf()
# 
# #how many have a non-missing orgunituid, meaning they did match
# xwalk %>% 
#   group_by(country) %>% 
#   tally(!is.na(orgunituid))
# 
# ##look for missing characters
# utf8::utf8_print(unique(xwalk$facility), utf8 = FALSE)
# 
# utf8::utf8_print(unique(xwalk$sitename), utf8 = FALSE)
# 
# 
# #read in psm-MER data-----------------------------------------------
# 
#   df_mer <- read_csv("C:/Users/Josh/Documents/data/fy20_q1_v1/scm_output")
# 
# df_mer <- read_csv("../../data/fy20_q1_v1/scm_output/mer_fy20_q1_v1_site_scm.csv")
# 
# 
# 
# 
# 
# 
# ##
# 
# # 
# # g_stitch_xwalk <- function(file){
# #   
# #   sheetname <- googlesheets4::sheet_names(file) %>% 
# #     pluck(grep("Mapped", .))
# #   
# #   df <- googlesheets4::read_sheet(file,
# #                           sheet = sheetname,
# #                           col_types= c(.default = "c")) %>%
# #     dplyr::mutate(country = basename(file)) %>% 
# #     dplyr::rename_all(~tolower(.))
# #   
# #   return(df)
# # }
# # 
# # #test it
# # test_x <- purrr::map_dfr(.x = drive_files,
# #                          .f = ~ g_stitch_xwalk(.x))
# 
# 
# 
# 
# 
# 
# 
# 

