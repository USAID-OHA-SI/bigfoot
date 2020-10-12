## angola test

## for miriam, 9.8.2020

## update: From LMIS: mot_ami for June for all ARVs
## from MER: FY20q3 TX_CURR and Q2 SC_ARVDISP; ie fy20_cumulative
## pick up here from steps 02-04

#munge mer data using `df_mer`

df_mer_munge <- df_mer %>%
  filter(country == "angola") %>% 
  mutate(period = "fy20_cumulative")


## bring in lmis and munge
df_reg_ang <- df_regimen %>% 
  filter(country == "angola") %>% 
  select(-snl1, -snl2) %>%
  group_by(country, facility, indicator, period,regimen_type_mer ) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>%
  rename(otherdisaggregate = regimen_type_mer) %>% 
  filter(period == "2020-07",
         indicator == "ami")

## merge them

ang_join <- df_mer_munge %>% 
  left_join(xwalk) %>% 
    bind_rows(df_reg_ang)
  

ang_join %>% write_csv(file.path(data_out, "angola_xwalked_q3_july.csv"))



