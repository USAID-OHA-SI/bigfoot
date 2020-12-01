## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  Create MER fy20 target file for SCH use case in DDC
## DETAIL :  FY20 sitelevel targets at total numerator level for TX_CURR, PrEP_CURR, TB_PREV, HTS_TST_POS
## Update:   10.23 update. Will add Q1-Q3 and TX_MMD disags


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

lmis_ous  <- c("Angola",
               "Botswana",
               "Cameroon",
               "Haiti",
               "Lesotho",
               "Malawi",
               "Mozambique",
               "Namibia",
               "Nigeria",
               "Uganda",
               "Zambia",
               "Zimbabwe")

indc <- c("PrEP_CURR", "TB_PREV", "HTS_TST", "HTS_TST_POS")

mer <- "C:/Users/Josh/Documents/data/fy20_q4_v1/site_ddc"

#munge and stitch--------------------------------------------------

##break this up into two parts
# creating rdss first keeps us from running into the memory error w function

raw_msds <- dir(mer, pattern = "*.zip", full.names = TRUE)

purrr::map(.x = msds,
           .f = ~ICPIutilities::read_msd(.x, save_rds = TRUE))

#function to create ddc compliant dataset
stitch_ddc <- function(file) {
  df <- readr::read_rds(file) %>%
    filter(fiscal_year == "2020") %>% 
    ICPIutilities::reshape_msd("long") %>%
    filter(period == "fy2020q4",
           indicator == "TX_CURR" & (
             is.na(trendscoarse) | trendscoarse == "15+" &
             standardizeddisaggregate %in%
             c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")) |
             indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>%
    filter(standardizeddisaggregate != "KeyPop/HIVStatus") %>% 
    group_by(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
             otherdisaggregate, indicator, period) %>%
    summarise(value = sum(val)) %>%
    ungroup()
  
}

#create df
msds <- dir(mer, pattern = "*.rds", full.names = TRUE)

df_ddc <- purrr::map_dfr(.x = msds,
                         .f = ~ stitch_ddc(.x))


#write it

df_ddc %>% write_csv(file.path(data_out, "mer_sch_ddc_fy20q4_v1.csv"))

#test

df_ddc %>% distinct(period)
df_ddc %>% distinct(indicator, period) %>% arrange(period)
df_ddc %>% distinct(operatingunit)
df_ddc %>%
  group_by(operatingunit, indicator, period) %>% 
  summarise(val = sum(value)) %>% prinf()

df_ddc %>% distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
  arrange(indicator) %>% 
  prinf()


file <- "C:/Users/Josh/Documents/data/fy20_q4_v1/site_ddc/MER_Structured_Datasets_Site_IM_FY18-21_20201113_v1_1_Angola.rds"
#original
df <- ICPIutilities::read_msd(file) %>%
  ICPIutilities::reshape_msd("long") %>%
  filter(indicator == "TX_CURR" & standardizeddisaggregate %in%
           c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus") |
           indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>%
  group_by(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
           otherdisaggregate, indicator, period) %>%
  summarise(value = sum(val)) %>%
  ungroup()

#w/age
#for some reason, this removes total numerator for tx-curr
df <- ICPIutilities::read_msd(file) %>%
  ICPIutilities::reshape_msd("long") %>%
  filter(indicator == "TX_CURR" & standardizeddisaggregate %in%
           c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")
         & trendscoarse != "<15" & trendscoarse != "Unknown Age" |
           indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>% 
  group_by(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
           otherdisaggregate, indicator, period) %>%
  summarise(value = sum(val)) %>%
  ungroup()

#take 3?
df <- ICPIutilities::read_msd(file) %>%
  ICPIutilities::reshape_msd("long") %>%
  filter(indicator == "TX_CURR" &
           is.na(trendscoarse) | trendscoarse == "15+" &
           standardizeddisaggregate %in%
           c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")|
           indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>%
  filter(standardizeddisaggregate != "KeyPop/HIVStatus") %>% 
  group_by(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
           otherdisaggregate, indicator, period) %>%
  summarise(value = sum(val)) %>%
  ungroup()

#take 4
df <- readr::read_rds(file) %>%
  filter(fiscal_year == "2020") %>% 
  ICPIutilities::reshape_msd("long") %>%
  filter(period == "fy2020q4",
         indicator == "TX_CURR" & (
           is.na(trendscoarse) | trendscoarse == "15+" &
           standardizeddisaggregate %in%
           c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")) |
           indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>%
  filter(standardizeddisaggregate != "KeyPop/HIVStatus") %>% 
  group_by(sitename, orgunituid, operatingunit, snu1, psnu, standardizeddisaggregate,
           otherdisaggregate, indicator, period) %>%
  summarise(value = sum(val)) %>%
  ungroup()

#test
df %>%
  filter(indicator == "TX_CURR") %>% 
  group_by(standardizeddisaggregate, indicator, period) %>% 
  summarise(val = sum(value)) %>% prinf()

df %>%
  group_by(indicator, standardizeddisaggregate) %>% 
  summarise(val = sum(value)) %>% prinf()

df %>%
  filter(indicator == "TX_CURR") %>% 
  group_by(operatingunit, indicator, standardizeddisaggregate, trendscoarse) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)


df %>% distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
  arrange(indicator) %>% 
  prinf()


df %>% distinct(indicator, period) %>% arrange(indicator)
df %>% distinct(operatingunit)
df %>% distinct(indicator, standardizeddisaggregate) %>% arrange(indicator)
df %>% distinct(indicator, standardizeddisaggregate) %>% arrange(indicator)


df %>%
  filter(indicator == "TX_CURR") %>% 
  group_by(standardizeddisaggregate, indicator, period) %>% 
  summarise(val = sum(value))


df %>%
  filter(indicator == "TX_CURR") %>% 
  group_by(operatingunit, indicator, standardizeddisaggregate) %>% 
summarise_if(is.numeric, sum, na.rm = TRUE)



