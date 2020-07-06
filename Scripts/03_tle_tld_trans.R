## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  create sc_arvdisp comparator
## DETAIL :  modify sc_fact for comparison with SC_ARVDISP

#Dependancies---------------------------------------------------
library(tidyverse)
library(vroom)
library(readxl)

#Globals----------------------------------------------------------
data_in <- "Data"
data_out <- "C:/Users/Josh/Documents/data/fy20_q2_v1/psm"
images <- "Images"

prinf <- function(df) {
  print(df, n = Inf)
}

prods <- c("Dolutegravir/Lamivudine/Tenofovir DF 50/150/300 mg Tablet, 30 Tabs",
           "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tabs",
           "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 30 Tabs",
           "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tabs",
           "Efavirenz/Lamivudine/Tenofovir DF 400/300/300 mg Tablet, 30 Tabs",
           "Efavirenz/Lamivudine/Tenofovir DF 400/300/300 mg Tablet, 90 Tabs",
           "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 180 Tabs",
           "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 30 Tabs",
           "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 90 Tabs")



#read in data----------------------------------------------------

#sc_fact
df <- vroom(file.path(data_out, "2020-03_SC-FACT_Data_.csv")) %>% 
  rename_all(~tolower(.))

#create indicator field
df_long <- df %>% 
  gather(indicator, value, soh:mos, na.rm = TRUE)

#product list

df_prods <- read_excel(file.path(data_out, "Data Triang. & DDC Next Steps - Action Plan.xlsx"),
                       sheet = "Q2 analysis product list")

#munge-----------------------------------------------------------
#sc_fact: keep ami only and filter for prods we want

df_long <- df_long %>% 
  filter(indicator == "ami",
         product %in% prods)

#munge df for the prods
df_prods <- df_prods %>% 
  gather(indicator, value, mot)

#join-----------------------------------------------------------

df_all <- dplyr::full_join(df_long,df_prods, by = "product")

df_all %>% write_csv(file.path(data_out, "sc_fact_tle_tld_comp.csv"))
  
  
  
  
  
  
  
  
  
  

  




