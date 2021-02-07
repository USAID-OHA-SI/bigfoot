## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  create cameroon xwalk file
## DETAIL :  
## Date   : 7.28.2020

#libraries---------------------------------------------------------

library(tidyverse)
library(readxl)
library(ICPIutilities)
library(glamr)

#globals-----------------------------------------------------------

mer <- "C:/Users/Josh/Documents/data/fy20_q2_v1/site_level"

data_in <- "Data"

#read in/munge-----------------------------------------------------

df_sc <- file.path(data_in, "2020-04_SC-FACT_Data.csv") %>% 
  vroom() %>% 
  rename_all(~tolower(.)) %>% 
  filter(country == "Cameroon",
         period == "2020-03") %>% 
  distinct(country, snl1, snl2, facility) %>% 
  arrange(facility)


df_mer <- read_rds(file.path(mer_site, "MER_Structured_Datasets_Site_IM_FY18-20_20200626_v2_1_Cameroon.rds")) %>% 
  distinct(operatingunit, sitename, snu1, psnu, orgunituid)

#write
df_sc %>% write.csv(file.path(data_out, "cameroon_2020.03_sc_fact_sites.csv"), fileEncoding = "UTF-16LE")

df_mer %>% write.csv(file.path(data_out, "cameroon_q2_clean_mer_sites.csv"), fileEncoding = "UTF-16LE")










