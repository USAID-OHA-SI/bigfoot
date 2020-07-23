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

#Globals----------------------------------------------------------
data_in <- "Data"
data_out <- "C:/Users/Josh/Documents/data/fy20_q2_v1/psm"
images <- "Images"

arvs <- c("Abacavir 300 mg, Tablet, 60 Tabs",
  "Abacavir/Lamivudine 600/300 mg Dispersible Tablet, 30 Tabs",
  "Abacavir/Lamivudine 600/300 mg Tablet, 30 Tablets",
  "Abacavir/Lamivudine 600/300 mg Tablet, 30 Tabs",
  "Atazanavir/Ritonavir 300/100 mg Tablet, 30 Tablets",
  "Atazanavir/ritonavir 300/100 mg Tablet, 30 Tabs",
  "Darunavir 600 mg Tablet, 60 Tabs",
  "Dolutegravir 50 mg Tablet, 30 Tabs",
  "Dolutegravir 50 mg Tablet, 30 Tabs",
  "Dolutegravir/Lamivudine/Tenofovir DF 50/150/300 mg Tablet, 30 Tabs",
  "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tabs",
  "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 30 Tabs",
  "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tabs",
  "Efavirenz 600 mg Tablet, 30 Tablets",
  "Efavirenz 600 mg Tablet, 30 Tabs",
  "Efavirenz/Emtricitabine/Tenofovir DF 600/200/300 mg Tablet, 30 Tabs",
  "Efavirenz/Lamivudine/Tenofovir DF 400/300/300 mg Tablet, 30 Tabs",
  "Efavirenz/Lamivudine/Tenofovir DF 400/300/300 mg Tablet, 90 Tabs",
  "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 180 Tabs",
  "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 30 Tablets",
  "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 30 Tabs",
  "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 90 Tabs",
  "Emtricitabine/Tenofovir DF 200/300 mg Tablet, 30 Tabs",
  "Emtricitabine/Tenofovir DF 300/300 mg Tablet, 30 Tabs",
  "Etravirine 100 mg Tablet, 120 Tabs",
  "Lamivudine 150 mg Tablet, 60 Tabs",
  "Lamivudine/Tenofovir DF 300/300 mg Tablet, 30 Tablets",
  "Lamivudine/Tenofovir DF 300/300 mg Tablet, 30 Tabs",
  "Lamivudine/Zidovudine 150/300 mg Tablet, 60 Tablets",
  "Lamivudine/Zidovudine 150/300 mg Tablet, 60 Tabs",
  "Lopinavir/ritonavir 200/50 mg Tablet, 120 Tablets",
  "Lopinavir/ritonavir 200/50 mg Tablet, 120 Tabs",
  "Nevirapine/Lamivudine/Zidovudine 200/150/300 mg Tablet, 60 Tablets",
  "Nevirapine/Lamivudine/Zidovudine 200/150/300 mg Tablet, 60 Tabs",
  "Raltegravir 400 mg Tablet, 60 Tabs")


#read in data----------------------------------------------------
# read in data SC_FACT data

df <- file.path(data_in, "2020-04_SC-FACT_Data.csv") %>% 
  vroom() %>% 
  rename_all(~tolower(.))

## first read in file that adds mot, etc..

df_prods <- file.path(data_in, "Data Triang. & DDC Next Steps - Action Plan.xlsx") %>% 
  read_excel(sheet = "regimen_pill_count") %>% 
  rename(regimen_type = `regimen type`,
         pill_count = `pill count`)

## filter SC_fact data to just arvs in arvs

df <- df %>% 
  filter(product %in% arvs,
         country != "Cameroon")

##join products and sc_fact

df_merged <- left_join(df, df_prods, by = "product")

# add mot
## want to keep only period 2020-03 for comparing to FY20q3

df_merged <- df_merged %>% 
mutate(mot_ami = ami*mot_adult,
       mot_soh = soh*mot_adult) %>% 
  mutate(mot_ami = case_when(country %in% c("Zimbabwe", "Haiti") ~ ((ami / pill_count)*mot_adult), TRUE ~ mot_ami),
         mot_soh = case_when(country %in% c("Zimbabwe", "Haiti") ~ ((soh / pill_count)*mot_adult),TRUE ~ mot_soh)) %>% 
  filter(period == "2020-03")

#create indicator field and select
df_long <- df_merged %>% 
  gather(indicator, value, colnames(select_if(., is.numeric)), na.rm = TRUE) %>% 
  select(-facilitycd, -datimcode, -facility_mapped, -source, -`datim facility`) %>% 
  filter(!is.na(regimen_type)) %>%
  mutate_at(vars(country, facility), ~tolower(.))

# Check for duplicates, flag those groups with more than 1 unique rows, then filter
# to remove the duplicates so we get a better merge
df_long <- 
  df_long %>% 
  group_by(facility, country, period, product, indicator, value) %>% 
  mutate(n = n(),
         dup_flag = row_number()) %>% 
  ungroup()


df_long %>% filter(n > 1) %>% View()

#remove duplicate obs
df_long_dedup <- df_long %>% filter(n ==1)

df_long_dedup %>% filter(n > 1) %>% View()

#remove flags
df_long_dedup <- df_long_dedup %>% 
  select(-n, -dup_flag)


# Check if our filter worked
dim(df_long)[1] - dim(df_long_dedup)[1]


## df_long now has all the thigns we want to merge
## join on the 'sitename' which is the datim site name from the crosswalked files
## 'facility' from SC_FACT is capitalized in some cases

  df_xwalked <- left_join(df_long_dedup, xwalk)



#merge take 2, vectorized---------------------------------------------------------
# #try using map to do each ou at a time
# 
# #create a list of each ou's df
#   df_list <- df_long_dedup %>%
#     group_split(country)
# 
#   df_xwalked  <- map_dfr(df_list, ~left_join(., xwalk, by = c("facility", "country")))

# check merged file for duplicates
dup_check <- df_xwalked %>% 
  group_by(country, facility, product, indicator, value) %>% 
  mutate(n = n(),
         dup_flag = row_number()) %>% 
  ungroup()

#examine
dup_check %>% filter(n > 1) %>% View()

#clean up for merging w MER

df_xwalked <- df_xwalked %>% 
  select(-snl1, -snl2) %>% 
  mutate(value = as.numeric(value))





#clean up workspace-------------------------------------------------------------------
rm(df_prods, df_list, df_long, df_merged, dup_check)

#examine/scratch----------------------------------------------------------------------
#check out angola

map(list(df_long, xwalk), ~glimpse(.))
intersect(unique(df_long$facility), unique(xwalk$facility)) %>% length()


map(df_long %>% select(contains("snl")), ~sum(is.na(.)))

map(xwalk, ~sum(is.na(.)))

#the 'joined' file
  df_xwalked %>%
    filter(country == "angola") %>%
    distinct(facility, sitename) %>%
    arrange(facility) %>% 
    prinf()

test1 %>%
  filter(country == "angola") %>%
  distinct(facility, sitename) %>%
  arrange(facility) %>% 
  prinf()

#the original crosswalk reference file
xwalk %>%
  filter(country == "angola") %>%
  distinct(facility, sitename) %>%
  arrange(facility) %>% 
  prinf()

#the sc_fact data
df_long %>%
  filter(country == "angola") %>%
  distinct(facility) %>%
  arrange(facility) %>% 
  prinf()


## examine results of munge
 df_xwalked %>% 
   group_by(country) %>% 
   tally(!is.na(sitename))
 
 df_xwalked %>% 
   count(country, period, is.na(sitename))
 
 df_xwalked %>% 
   mutate(flag = if_else(is.na(sitename), 1, 0)) %>% 
   group_by(country) %>% 
   count(flag)
 
#check duplicates
 tmp <- df %>% 
   filter(facilitycd == "200966",
          period == "2020-03")
 

 xwalk %>%
   filter(country == "angola") %>%
   distinct(facility, sitename) %>%
   arrange(facility) %>% 
   prinf()
 
 df_xwalked %>%
   filter(country == "angola") %>%
   distinct(facility) %>%
   arrange(facility) %>% 
   prinf()
 
 glimpse(df_long)
 glimpse(xwalk)
 
#look for hidden characters
 
 xwalk %>% 
   filter(country == "angola") %>% 
   utf8::utf8_print(unique(facility), utf8 = FALSE)

#write test data 7.13.20
 df_long %>% write_csv(file.path(data_out, "sc_fact_data_7.13.20.csv"))
 xwalk %>% write_csv(file.path(data_out, "xwalk_reference_file.csv"))
 

 ##

 test <- df %>% group_by(facility, product, period) %>% 
   mutate(n = n(),
          id = row_number())
 
test %>% filter(n>1) %>% arrange(facility, product, period) %>% view()
 
test2 <- df %>% group_by(country, snl1, snl2, facility, product, period) %>% 
  mutate(n = n(),
         id = row_number()) 

test2 %>% filter(n>1) %>% arrange(snl1, snl2, facility, product, period) %>% view()

test2 %>% filter(n>1) %>%
  arrange(country, period, facility, product) %>% 
  write_csv(file.path(data_out, "sc_fact_april_duplicates.csv"))





  
  
  
  
  
  
  
  

  




