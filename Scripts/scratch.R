## scratch

##make 1 file from the two they sent

df1 <- readr::read_csv("C:/Users/Josh/Documents/GitHub/bigfoot/Data/Standardized_Processed_Data_2020.csv",
                       col_types = c(.default = "c"))

df2 <- readr::read_csv("C:/Users/Josh/Documents/GitHub/bigfoot/Data//Standardized_Processed_Data_2019.csv",
                       col_types = c(.default = "c"))


# ## as of 2/1/21 version from PSM, below encoding workaround is depreciated
# df2 <- readr::read_csv("C:/Users/Josh/Documents/GitHub/bigfoot/Data//Standardized_Processed_Data_2019.csv",
#                        col_types = c(.default = "c"),
#                        locale = readr::locale(encoding = "latin1"))




big <- bind_rows(df1, df2)

big %>% write_csv(here(Dataout, "sc_fact_2.1.21_combined.csv"))

old <- read_csv("C:/Users/Josh/Desktop/bigfoot copy/data/sc_fact/Processed_Data_2019.csv")


xwalk %>%
  group_by(country) %>%
  tally(!is.na(datim_orgunituid))


xwalk %>%
  mutate(no_lmis_match = ifelse(is.na(lmis_facility), "no lmis site", "lmis site")) %>%
  group_by(country, no_lmis_match) %>%
  spread(no_lmis_match, country)


test <- xwalk %>%
  filter(!orgunituid %in% remove)

test %>%
  summarise(unique_datimfac = n_distinct(site_name),
            unqiue_datimuid = n_distinct(orgunituid),
            unique_lmis = n_distinct(lmis_facility))

test %>% filter(is.na(site_name),
                !is.na(orgunituid)) %>% view()

xwalk %>%
  filter(!orgunituid %in% remove,
         country == "Zambia",
         is.na(site_name)) %>% 
  distinct(orgunituid)
  

xwalk %>%
  filter(!orgunituid %in% remove,
         country == "Zambia",
         is.na(site_name),
         !is.na(orgunituid)) %>% view()


xwalk %>%
  filter(country == "Zambia") %>% 
  summarise(unique_datimfac = n_distinct(site_name),
            unqiue_datimuid = n_distinct(orgunituid),
            unique_lmis = n_distinct(lmis_facility))


xwalk %>% 
  filter(is.na(datim_facility)) %>% 
  distinct(country, datim_orgunituid)

xwalk %>%
  filter(country == "Zambia" & !is.na(site_name) & !is.na(datim_community)) %>%
  distinct(site_name, orgunituid)
  
  summarise(unique_datimfac = n_distinct(site_name),
            unqiue_datimuid = n_distinct(orgunituid),
            unique_lmis = n_distinct(lmis_facility))


 
xwalk %>%
  filter(is.na(site_name))%>%
  distinct(country, datim_community) %>% prinf()


xwalk %>%
  filter(is.na(site_name))%>%
  summarise(unique_datimfac = n_distinct(site_name),
            unqiue_datimuid = n_distinct(orgunituid),
            unique_lmis = n_distinct(lmis_facility))


## look at bots, why is there 107 instead of 108?
xwalk %>% 
  filter(country == "Botswana") %>% 
  distinct(datim_facility) %>% arrange(datim_facility) %>% 
  prinf()


##scratch looking at sc-fact

##sc fact

sc_fact <- "C:/Users/Josh/Documents/data/fy20_q4_v1/psm/Processed_Data_4_countries.csv"

processed_data <- read_csv(sc_fact)

x <- "C:/Users/Josh/Documents/data/fy20_q4_v1/psm/Crosswalk Dictionary USAID.xlsx"

crosswalk_dictionary <- readxl::read_xlsx(x)

glimpse(processed_data)
glimpse(crosswalk_dictionary)


## how many facilities by OU
#crosswalk dictionary
crosswalk_dictionary %>%
  group_by(Country) %>%
  summarise(unique_lmis = n_distinct(LMIS_Facility),
            unique_datim = n_distinct(DATIM_facility)) %>% 
  mutate(coverage = unique_datim/unique_lmis)

processed_data %>%
  group_by(Country) %>%
  summarise(unique_lmis = n_distinct(Facility),
            unique_datim = n_distinct(DATIM_facility))

##matches etc.
# #how many have a non-missing orgunituid, meaning they did match

crosswalk_dictionary %>%
  filter(Country %in% c("Haiti", "Namibia", "Nigeria", "Zimbabwe")) %>%
  group_by(Country) %>%
  tally(!is.na(DATIM_orgunitid))

proc_mod <- processed_data %>%
  gather(indicator, value, colnames(select_if(., is.numeric)), na.rm = TRUE) %>%
  group_by(Country, Facility, DATIM_facility, DATIM_orgunitid) %>%
  tally(!is.na(DATIM_orgunitid))




summarise_if(is.numeric, ~ sum(., na.rm = TRUE))

tally(!is.na(DATIM_orgunitid))




%>%
  filter(Country %in% c("Haiti", "Namibia", "Nigeria", "Zimbabwe")) %>%
  group_by(Country) %>% 
  summarise(n_distinct(c("DATIM_facility", "LMIS_Facility")))

df %>%
  group_by(Country) %>% 
  summarise(n_distinct(DATIM_facility))

xwalk %>%
  filter(Country %in% c("Haiti", "Namibia", "Nigeria", "Zimbabwe")) %>%
  group_by(Country) %>% 
  summarise(n_distinct(LMIS_Facility))

df %>%
  group_by(Country) %>% 
  summarise(n_distinct(Facility))

xwalk %>%
  
  
  
  

#FROM TIM

tx_tbl <- tmp_prf %>% 
  select(1:7) %>% 
  gt(rowname_col = "IP",
     groupname_col = "indicator") %>% 
  fmt_missing(columns = everything(), missing_text = " ") %>% 
  tab_header(title = "TREATMENT: USAID Implementing Mechanism Performance") %>% 
  tab_options(table.font.names = "Source Sans Pro")  %>% 
  fmt_number(columns = 2:7, decimals = 0, rows = 5) %>% 
  fmt_percent(columns = 2:7, decimals = 0, 
              rows = 1:4) %>% 
  tab_style(style = list(cell_text(weight = 'bold')), 
            locations = cells_body(columns = 1:7, 
                                   rows = `USAID Implementing Mechanism` == "USAID")) %>% 
  cols_width(
    everything() ~ px(120)) %>% 
  tab_footnote(
    footnote = md("*No target set for MMD. Goal was to scale up as rapidly as possible*"),
    locations = cells_column_labels(
      columns = contains("MMD")
    )
  )gt



##scratch
## look at data sent 1/11/21

df1 <- read_csv(file.path(data_in, "sc_fact/Processed_Data_2019.csv"))
df2 <- read_csv(file.path(data_in, "sc_fact/Processed_Data_2020.csv")) 

dfall <- bind_rows(df1, df2) 

dfall %>% write_csv(file.path(data_out, "sc_fact_processed_all.csv"))



xwalk <- "C:/Users/Josh/Documents/data/fy20_q4_v2/sc_fact/crosswalk_usaid.xlsx"

##crosswalk sent on 1/11
crosswalk <- readxl::read_xlsx(xwalk)


crosswalk %>% 
  group_by(Country) %>%
  summarise(unique_lmis = n_distinct(LMIS_Facility),
            unique_datim = n_distinct(DATIM_orgunitid)) %>% 
  mutate(coverage = unique_datim/unique_lmis)

df %>% 
  group_by(country) %>% 
  summarise(unique_lmis = n_distinct(facility),
            unique_datim = n_distinct(datim_orgunituid)) %>% 
  mutate(coverage_scfact = unique_datim/unique_lmis)



df %>% 
  filter(indicator == "ami",
         age_group == "peds") %>% 
  group_by(country, period) %>% 
  summarise(val = sum(value))

df %>% 
  filter(productcategory == "Pediatric ARV") %>% 
  distinct(age_group, product) %>% 
  arrange(product) %>% 
  prinf()









