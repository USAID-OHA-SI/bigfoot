## PROJECT:  bigfoot
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  read in MER data and munge
## DETAIL :  Read in site level data for 12 OUs and change to format for merging with SC_FACT

#Dependancies-------------------------------------------------
#where are your MER site level datasets - for 12 LMIS ous only
mer <- "C:/Users/Josh/Documents/data/fy20_q3_v1/site_level_for_lmis"

# do you need to subset by indicator?

# list of LMIS OUs if you need them
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

#Libraries-------------------------------------------------


#munge--------------------------------------------------------
# create site level MER data

listy <- dir(mer, pattern = "*.txt", full.names = TRUE)

# function is set up to get 
get_scfact_mer <- function(input) {

  df <- ICPIutilities::read_msd(input) %>% 
    filter(indicator %in% c("TX_CURR", "SC_ARVDISP"),
           indicator == "TX_CURR" & standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus",
                                                                    "Total Numerator") |
             indicator == "SC_ARVDISP")
}

df_mer_raw <- purrr::map_dfr(.x = listy,
                         .f =~ get_scfact_mer(.x))
## check output

df_mer_raw %>% 
  distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>% arrange(indicator) %>% prinf()

#clean up
df_mer <- df_mer_raw %>%
  filter(operatingunit %in% lmis_ous) %>% 
  dplyr::group_by_at(vars(-primepartner, -fundingagency, -mech_code, -mech_name,
                          -pre_rgnlztn_hq_mech_code, -prime_partner_duns, -award_number)) %>%
  dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  dplyr::ungroup()

#one more reshape to get rid of disags, etc..
df_mer <- df_mer %>% 
  reshape_msd("long") %>%
  filter(period == "fy2020cumulative") %>% 
    group_by(sitename, operatingunit, orgunituid, snu1, psnu, indicator, standardizeddisaggregate, otherdisaggregate) %>% 
  summarise(val = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(country = operatingunit,
         value = val) %>% 
  mutate(country = tolower(country))



#end-----------------------------------------------------------------------------------
df_mer %>% 
  distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>% prinf()

#splay both df's wide, drop pd---------------------------------------------

df_mer_w <- df_mer %>% 
  spread(indicator, value)

df_regimen_w <- df_regimen %>% 
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
  full_join(df_regimen_w)

df_mer_w %>% write_csv(file.path(data_out, "mer_q2_wide_sc_xwalk.csv"))
df_long_dedup_w %>% write_csv(file.path(data_out, "sc_fact_2020.03.wide.csv"))


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



