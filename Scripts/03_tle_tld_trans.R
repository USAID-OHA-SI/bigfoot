## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  create sc_arvdisp comparator
## DETAIL :  modify sc_fact for comparison with SC_ARVDISP

#Dependancies---------------------------------------------------
library(tidyverse)
library(vroom)
library(readxl)
library(glamr)
library(googledrive)
library(ICPIutilities)
library(googlesheets4)
library(here)


#Globals----------------------------------------------------------
data_in <- "Data"
data_out <- "Dataout"
images <- "Images"

  
#read in data----------------------------------------------------
# read in data SC_FACT data

df <- file.path(data_in, "SC-FACT Data 2020-06 Updated.csv") %>% 
  vroom() %>% 
  rename_all(~tolower(.))

## filter SC_fact data to just arvs in arvs

df <- df %>% 
  filter(productcategory %in% c("ARV", "Adult ARV", "Pediatric ARV"))

## first read in file that adds mot, etc..
## make this dynamic; read from google drive

## establish connection and authenticate
user_drive <- "jodavis@usaid.gov" #USAID email
drive_auth(user_drive)

# which file are we using?
g_file <- "1O-rwWWp-8GsbqWhfcr01S9glMazAdEGvdkwhMZZcf0Y"

df_g_drive <- read_sheet(g_file,
                         sheet = "updated_regimen",
                         col_types= c(.default = "c"))

df_prods <-  df_g_drive %>%
  filter(is.na(`include in analysis?`)) %>% 
  select(-`include in analysis?`, -`...11`, -`...12`, -added) %>% 
  rename_all(~tolower(.)) %>% 
  mutate(mot = as.numeric(mot))
  
##join products and sc_fact

df_merged <- left_join(df, df_prods, by = "product")

# ## check merged for duplicates
# dim(df_merged)[1] - dim(df)[1]
# #10,360 extra records created in 8.28.20 updated version

## add mot

df_merged <- df_merged %>%
  mutate(mot_ami = ami*mot,
         mot_soh = soh*mot)

# DEPRECIATED : 8.21 per cody
# %>% 
#   mutate(mot_ami = case_when(country %in% c("Zimbabwe", "Haiti") ~ ((ami / pill_count)*mot_adult), TRUE ~ mot_ami),
#          mot_soh = case_when(country %in% c("Zimbabwe", "Haiti") ~ ((soh / pill_count)*mot_adult),TRUE ~ mot_soh))

#create indicator field and select
df_long <- df_merged %>% 
  gather(indicator, value, colnames(select_if(., is.numeric)), na.rm = TRUE) %>% 
  select(-facilitycd, -datimcode, -facility_mapped, -source, -`datim facility`)

# Check for duplicates, flag those groups with more than 1 unique rows, then filter
# to remove the duplicates so we get a better merge
df_long <- df_long %>% 
  group_by(facility, snl1, country, period, product, indicator, value) %>% 
  mutate(n = n(),
         dup_flag = row_number()) %>% 
  ungroup()

## look at duplicates

df_long %>%
  filter(n > 1) %>%
  arrange(facility) %>% 
  view()

df_long %>%
  filter(n > 1) %>%
  arrange(period) %>% 
  distinct(period, country) %>% prinf()

  
#remove duplicate obs
df_long_dedup <- df_long %>% filter(n ==1)

#remove flags
df_long_dedup <- df_long_dedup %>% 
  select(-n, -dup_flag)


# Check if our filter worked
dim(df_long)[1] - dim(df_long_dedup)[1]

##  7.28.20
##  collapse on regime_type

# df_regimen <- df_long_dedup %>% 
#   group_by(country, snl1, snl2, facility, regimen_type, indicator) %>% 
#   summarise(value = round(sum(value, na.rm = TRUE),0)) %>% 
#   filter(value !=0)

## updated 8.25 for new vars
df_regimen <- df_long_dedup %>%
  group_by(country, regimen_type_mer, age_group, regimen_optimized,
           combination_type, indicator, period) %>%
  summarise(value = round(sum(value, na.rm = TRUE),0)) %>% 
  filter(value !=0)
  
  
#write-----------------------------------------------------------------------------
df_regimen %>% write_csv(file.path(data_out, "lmis_may_collapsed_8.31.20.csv"))

#clean up workspace----------------------------------------------------------------
rm(df, df_g_drive, df_long, df_merged, df_prods)

## df_long now has all the thigns we want to merge
## join on the 'sitename' which is the datim site name from the crosswalked files
## 'facility' from SC_FACT is capitalized in some cases

#df_xwalked <- left_join(df_long_dedup, xwalk)



#merge take 2, vectorized---------------------------------------------------------
# #try using map to do each ou at a time
# 
# #create a list of each ou's df
#   df_list <- df_long_dedup %>%
#     group_split(country)
# 
#   df_xwalked  <- map_dfr(df_list, ~left_join(., xwalk, by = c("facility", "country")))

# # check merged file for duplicates
# dup_check <- df_xwalked %>% 
#   group_by(country, facility, product, indicator, value) %>% 
#   mutate(n = n(),
#          dup_flag = row_number()) %>% 
#   ungroup()
# 
# #examine
# dup_check %>% filter(n > 1) %>% View()
# 
# #clean up for merging w MER
# 
# df_xwalked <- df_xwalked %>% 
#   select(-snl1, -snl2) %>% 
#   mutate(value = as.numeric(value))







# #examine/scratch----------------------------------------------------------------------
# #check out angola
# 
# map(list(df_long, xwalk), ~glimpse(.))
# intersect(unique(df_long$facility), unique(xwalk$facility)) %>% length()
# 
# 
# map(df_long %>% select(contains("snl")), ~sum(is.na(.)))
# 
# map(xwalk, ~sum(is.na(.)))
# 
# #the 'joined' file
#   df_xwalked %>%
#     filter(country == "angola") %>%
#     distinct(facility, sitename) %>%
#     arrange(facility) %>% 
#     prinf()
# 
# test1 %>%
#   filter(country == "angola") %>%
#   distinct(facility, sitename) %>%
#   arrange(facility) %>% 
#   prinf()
# 
# #the original crosswalk reference file
# xwalk %>%
#   filter(country == "angola") %>%
#   distinct(facility, sitename) %>%
#   arrange(facility) %>% 
#   prinf()
# 
# #the sc_fact data
# df_long %>%
#   filter(country == "angola") %>%
#   distinct(facility) %>%
#   arrange(facility) %>% 
#   prinf()
# 
# 
# ## examine results of munge
#  df_xwalked %>% 
#    group_by(country) %>% 
#    tally(!is.na(sitename))
#  
#  df_xwalked %>% 
#    count(country, period, is.na(sitename))
#  
#  df_xwalked %>% 
#    mutate(flag = if_else(is.na(sitename), 1, 0)) %>% 
#    group_by(country) %>% 
#    count(flag)
#  
# #check duplicates
#  tmp <- df %>% 
#    filter(facilitycd == "200966",
#           period == "2020-03")
#  
# 
#  xwalk %>%
#    filter(country == "angola") %>%
#    distinct(facility, sitename) %>%
#    arrange(facility) %>% 
#    prinf()
#  
#  df_xwalked %>%
#    filter(country == "angola") %>%
#    distinct(facility) %>%
#    arrange(facility) %>% 
#    prinf()
#  
#  glimpse(df_long)
#  glimpse(xwalk)
#  
# #look for hidden characters
#  
#  xwalk %>% 
#    filter(country == "angola") %>% 
#    utf8::utf8_print(unique(facility), utf8 = FALSE)
# 
# #write test data 7.13.20
#  df_long %>% write_csv(file.path(data_out, "sc_fact_data_7.13.20.csv"))
#  xwalk %>% write_csv(file.path(data_out, "xwalk_reference_file.csv"))
#  
# 
#  ##
# 
#  test <- df %>% group_by(facility, product, period) %>% 
#    mutate(n = n(),
#           id = row_number())
#  
# test %>% filter(n>1) %>% arrange(facility, product, period) %>% view()
#  
# test2 <- df %>% group_by(country, snl1, snl2, facility, product, period) %>% 
#   mutate(n = n(),
#          id = row_number()) 
# 
# test2 %>% filter(n>1) %>% arrange(snl1, snl2, facility, product, period) %>% view()
# 
# test2 %>% filter(n>1) %>%
#   arrange(country, period, facility, product) %>% 
#   write_csv(file.path(data_out, "sc_fact_april_duplicates.csv"))
# 
# 
# 
# 

  
  
  
  
  
  
  
  

  




