## test merge for Zim, nmb, hti, and nga
## create merge flag then look at share of tx_curr on merge/non merge

#libraries-------------------------------------------------------------
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

site_level <- "C:/Users/Josh/Documents/data/fy20_q3_v1/site_level_for_lmis"

#file <- "C:/Users/Josh/Documents/data/fy20_q3_v1/site_level/MER_Structured_Datasets_Site_IM_FY18-20_20200814_v1_1_Haiti.zip"

#read in MSDs-----------------------------------------------------
msds <- dir(site_level, pattern = "*.txt", full.names = TRUE)

stitch <- function(file) {
  df <- ICPIutilities::read_msd(file) %>% 
    ICPIutilities::reshape_msd("long") %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(sitename, orgunituid, operatingunit, snu1, psnu, indicator, period) %>% 
    summarise(value = sum(val)) %>%
    ungroup() %>% 
    filter(period == "fy2020_targets") %>%
    group_by(operatingunit) %>% 
    mutate(ou_tx_targ = sum(value)) %>% 
    unite(indc, c(indicator, period), sep = "_") %>% 
    spread(indc, value)
}

df_mer <- purrr::map_dfr(.x = msds,
                         .f = ~ stitch(.x))

#generate target share
df_all <- df_mer %>%
  filter(operatingunit %in% lmis_ous) %>% 
  group_by(operatingunit) %>% 
  mutate(total_share = round(TX_CURR_fy2020_targets/ou_tx_targ, 5)) %>% 
  ungroup()

#merge on xwalk

xwalk_mod <- xwalk %>% 
  rename(operatingunit = country)


df_bigfoot <- df_all %>% 
  full_join(xwalk)

df_bigfoot_mod <- df_all %>% 
  full_join(xwalk_mod)

#diagnostics

#create flag for obs w/o a lmis site var
df_bigfoot <- df_bigfoot %>% 
  mutate(no_lmis_match = ifelse(is.na(facility), "no lmis site", "lmis site"))

#analyze

df_bigfoot %>% 
  group_by(operatingunit, no_lmis_match) %>% 
  summarise(missing_tx = sum(total_share, na.rm = TRUE)) %>% 
  spread(no_lmis_match, missing_tx)

#sites matched v not

df_bigfoot %>% 
  group_by(operatingunit, no_lmis_match) %>% 
  count(no_lmis_match) %>% 
  spread(no_lmis_match, n)

#look at sites for angola 

df_bigfoot %>% filter(operatingunit == "Angola",
                      no_lmis_match == "lmis site") %>% 
  distinct(sitename, facility)

df_bigfoot %>% filter(operatingunit == "Haiti",
                      no_lmis_match == "lmis site") %>% 
  distinct(sitename, facility) %>% prinf()

#end------------------------------------------------------
#bring lat x longs

mer_coords <- readr::read_csv(file.path(data_in, "HFR_FY20_GLOBAL_orghierarchy_20200825.csv")) %>% 
  select(orgunituid, latitude, longitude)

df_bigfoot <- df_bigfoot %>% 
  left_join(mer_coords)

#write it out for tim/baboyma
df_bigfoot %>% 
  write_csv(file.path(data_out, "LMIS_txcurr_share_coords.csv"))

##checks

#look at overall LMIS data

df %>% 
  group_by(country) %>% 
  distinct(country, snl1, snl2, facility) %>% 
  write_csv(file.path(data_out, "sc_fact_complete_hierarchy.csv"))

df_bigfoot %>%
  count(no_lmis_match)

df %>% filter(country == "zimbabwe") %>% 
  distinct(snl1) %>% 
  

## recpde snl1 for zim


df_zim <- df %>%
  mutate_at(vars(snl1), ~ tolower(.)) %>% 
  mutate(snl1 = case_when(snl1 == "bulawayo  metropolitan  province" ~ "bulawayo",
                        snl1 == "matebeleland north" ~ "matabeleland north",
                        snl1 == "matebeleland south" ~ "matabeleland south",
                        snl1 == "harare metropolitan province" ~ "harare",
                        snl1 == "manicaland  province" ~ "manicaland",
                        snl1 == "mashonaland  east" ~ "mashonaland east",
                        snl1 == "mashonaland  central" ~ "mashonaland central",
                        TRUE ~ snl1))



df_mer %>% 
  filter(operatingunit == "Zimbabwe") %>% 
  distinct(snu1) 

df_zim %>% 
  filter(country == "zimbabwe") %>% 
  distinct(country, snl1, snl2, facility) %>% 
  write_csv(file.path(data_out, "sc_fact_complete_hierarchy_zim.csv"))











