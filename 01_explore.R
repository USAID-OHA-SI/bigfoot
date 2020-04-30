## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  structure project folders
## DETAIL :  inital exploratory look at LMIS data

#Dependancies-----------------------------------------------------
library(tidyverse)
library(vroom)


#Globals----------------------------------------------------------
data_in <- "Data"
data_out <- "C:/Users/Josh/Documents/data/fy20_q1_v2/scm/SC-FACT Data 2019-01 to 2020-01"
images <- "Images"

prinf <- function(df) {
  print(df, n = Inf)
}

#----------------------------------------------------------------
# read in data

df <- read_csv("C:/Users/Josh/Documents/data/fy20_q1_v2/scm/SC-FACT Data 2019-01 to 2020-01/SC-FACT Data 2019-01 to 2020-01.csv") %>% 
  rename_all(tolower(.))

df <- vroom("C:/Users/Josh/Documents/data/fy20_q1_v2/scm/SC-FACT Data 2019-01 to 2020-01/SC-FACT Data 2019-01 to 2020-01.csv") %>% 
  rename_all(~tolower(.))

glimpse(df)

distinct(df, country)

#create indicator field
df <- df %>% 
  gather(indicator, value, soh:mos, na.rm = TRUE)

## check cameroon

df %>% 
  filter(country == "Cameroon") %>% 
  

df %>% 
  group_by(period, country, indicator) %>%
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  write_csv(file.path(data_out, "sc_fact_summary.csv"))










