## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  structure project folders
## DETAIL :  inital exploratory look at LMIS data

#Dependencies-----------------------------------------------------
library(tidyverse)
library(vroom)
library(readxl)
library(fs)
library(glitr)
library(glamr)
library(ICPIutilities)


#Globals----------------------------------------------------------
data_in <- "Data"
data_out <- "Dataout"
images <- "Images"


#----------------------------------------------------------------
# read in data SC_FACT data

df <- file.path(data_in, "2020-04_SC-FACT_Data.csv") %>% 
  vroom() %>% 
  rename_all(~tolower(.))


#reshape long and create indicator field
#df_long <- df %>% 
  #gather(indicator, value, soh:mos, na.rm = TRUE)

#clean up workspace----------------------------------------------

#generic checks/scratch------------------------------------------
## check some things


df_long %>% 
  group_by(period, country, indicator) %>%
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  write_csv(file.path(data_out, "sc_fact_summary_2020_03.csv"))

df %>% 
  group_by(period, country, indicator) %>%
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  write_csv(file.path(data_out, "sc_fact_summary.csv"))

#look at product and productcategory
df_long %>% 
  filter(country == "Haiti",
         indicator == "soh") %>% 
  distinct(productcategory, product) %>%
  arrange(productcategory) %>% 
  prinf()

# look at where datimuid is missing
# How many facilities have a datimuid by OU
df_long_old %>%
  group_by(country, period) %>%
  summarise(unqiue_code = n_distinct(datimcode)) %>% prinf()
 
df_long_old %>%
  group_by(country) %>%
  summarise(unqiue_code = n_distinct(`datim facility`))

# any rows missing 'product'?
df_long %>%
  filter(indicator == "ami") %>% 
  group_by(country, period) %>% 
  summarise(unqiue_code = n_distinct(product)) %>% 
  spread(period, unqiue_code)

#angola only
df_long %>%
  filter(indicator == "ami",
         country == "Angola",
         productcategory == "ARV") %>% 
  group_by(product, period) %>% 
  summarise(unqiue_code = n_distinct(product)) %>% 
  spread(period, unqiue_code) %>% prinf()

df_long %>%
  filter(indicator == "soh") %>% 
  group_by(country, period) %>% 
  summarise(unqiue_code = n_distinct(product)) %>% 
  spread(period, unqiue_code)

## facilityCD and Datimuid
df_long %>%
  filter(indicator == "ami") %>% 
  group_by(country) %>% 
  summarise(unqiue_facilitycd = n_distinct(facilitycd),
            unique_datimcode = n_distinct(datimcode))

## look at soh
df_long %>%
  filter(indicator == "soh",
         period == "2019-03") %>% 
  group_by(country, period) %>% 
  summarise(value = sum(value)) %>%
  write_csv(file.path(data_out, "2020_03_fac_soh.csv"))


df_long %>% 
  group_by(country, period, indicator) %>% 
  summarise(value = sum(value))
  
# facilty count
df %>%
  group_by(country, period) %>%
  summarise(unqiue_code = n_distinct(facility, na.rm = TRUE)) %>%
  arrange(country, period) %>%
  spread(period, unqiue_code)
  write_csv(file.path(data_out, "2020_03_site_count.csv"))

  
  df_long %>% 
    filter(country == "Uganda") %>% 
    distinct(facility)

  
  
  # any rows missing 'product'?
  df_long %>%
    filter(indicator == "ami",
           country == "Zambia") %>% 
    group_by(product, period) %>% 
    summarise(unqiue_code = n_distinct(product)) %>% 
    spread(period, unqiue_code) %>% prinf()


