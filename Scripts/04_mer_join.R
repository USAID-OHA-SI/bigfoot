## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  read in and join MER data to SC_FACT
## DETAIL :  

#Dependancies-------------------------------------------------
mer <- "C:/Users/Josh/Documents/data/fy20_q2_v1/site_level"

indc <- c("TX_CURR", "TX_NEW")

#Libraries----------------------------------------------------


#munge--------------------------------------------------------
# create site level MER data

# makey_rds <- dir(mer, pattern = "*.rds", full.names = TRUE)
# 
# get_scfact_mer <- function(input) {
#   
#   df_mer <- readr::read_rds(input) %>% 
#     dplyr::filter(fiscal_year == 2020,
#                   (indicator %in% indc &
#                     numeratordenom == "N" & trendscoarse %in% c("<15", "15+")) |
#     (indicator %in% indc & 
#        standardizeddisaggregate == "Total Numerator") | 
#       indicator %in% c("SC_CURR", "SC_ARVDISP"))
# 
# }
# 
# 
# df_mer_raw <- purrr::map_dfr(.x = makey_rds, .f = ~get_scfact_mer(.x))
# 
# df_mer_raw %>% write_csv(file.path(data_out, "2020_tx_sc_mer.csv"))

df_mer_raw <- vroom::vroom(file.path(data_out, "2020_tx_sc_mer.csv"))

#clean up
df_mer <- df_mer_raw %>%
  dplyr::group_by_at(vars(-primepartner, -fundingagency, -mech_code, -mech_name,
                          -pre_rgnlztn_hq_mech_code, -prime_partner_duns, -award_number)) %>%
  dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  dplyr::ungroup()

#one more reshape to get rid of disags, etc..
df_mer <- df_mer %>% 
  reshape_msd("long") %>% 
  group_by(sitename, operatingunit, orgunituid, snu1, psnu, indicator, period) %>% 
  summarise(val = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(country = operatingunit,
         value = val) %>% 
  filter(period == "fy2020cumulative") %>% 
  mutate(country = tolower(country))

#splay both df's wide, drop pd---------------------------------------------

df_mer_w <- df_mer %>% 
  spread(indicator, value)

df_long_dedup_w <- df_regimen %>% 
  select(-period) %>% 
  spread(indicator, value)

# df_xwalked_w <- df_xwalked %>%
#   select(-period) %>% 
#   spread(indicator, value)


#join xwalk to mer--------------------------------------------------------------

# #create a list of each ou's df
# df_list <- df_xwalked %>% 
#   group_split(country) 
# 
# #map(df_list, ~left_join(., xwalk, by = c("facility", "country")))
# 
# df_sch_mer  <- map_dfr(df_list, ~left_join(., df_mer))

df_mer_w <- df_mer_w %>% 
  left_join(xwalk)

df_sch_mer <- df_mer_w %>% 
  left_join(df_long_dedup_w)

#write

df_sch_mer %>% write_csv(file.path(data_out, "mer_sch_combo_wide.csv"))

#examine/scratch-------------------------------------------------------------

#calculate share
group_by(country) %>% 
  mutate(share_d = sum(TX_CURR, na.rm = TRUE )) %>% 
  ungroup() %>% 
  mutate(share = TX_CURR/share_d) %>% 
  select(-share_d)
#share



glimpse(df_xwalked)
glimpse(df_mer)

df_mer %>%
  filter(operatingunit == "Mozambique") %>% 
  distinct(sitename) %>%
  arrange(sitename) %>% 
  print(n=100)

df_mer %>% 
  distinct(indicator)


df_mer_raw %>%
  filter(operatingunit == "Mozambique") %>% 
  distinct(sitename) %>%
  arrange(sitename) %>% 
  print(n=100)


df_merged %>%
  filter(country == "angola") %>% 
  distinct(facility, sitename) %>%
  arrange(facility) %>% 
  prinf()
