#join MER to xwalk by orgunituid

join_mer_xwalk <- function(filepath, filename) {
df1 <- df %>% 
  left_join(xwalk) 
}

df1 %>% write_csv(file.path(Dataout, "mer_lmis_tableau_output_fy20q4_v5.csv"))

#check if any MER/LMIS facilities are unmatched
df1 %>% filter(is.na(lmis_facility)) %>% View()
df1 %>% filter(is.na(orgunituid)) %>% View()