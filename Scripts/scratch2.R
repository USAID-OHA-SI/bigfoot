##scratch PPMR
##stopping 4.23.21
## the two dfs with the comparable metrics (TX_CURR and mot_soh) complete below
## need to figure out how to blend the two in the same df
## need to explore the tx_curr:mot ratio
## update 7.21.21 - changed ppmr to add quarters


library(extrafont)
library(scales)
install.packages("ggnewscale")
library(ggnewscale)
library(tidyverse)


#munge mer for PPMR
## use OU by IM
filename <- "MER_Structured_Datasets_OU_IM_FY19-21_20210618_v2_1.zip"

mer <- read_msd("~/GitHub/bigfoot/Data/mer_data/MER_Structured_Datasets_OU_IM_FY19-21_20210618_v2_1.zip")


##read in PPMR

#get_ppmr()
ppmr <- ppmr_df()



df_sc %>% 
  filter(product_category == "Adult ARV",
         indicator == "mot_soh") %>% 
  group_by(country, period) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% prinf()


df %>% 
  filter(indicator == "soh",
         product == "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 30 Tablets",
         period == "2021-01") %>% 
  group_by(product, country, period) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = product, values_from = value)

#check if mot = mot_soh or if any mot are missing
df %>% 
  filter(product_type == "ARV",
         indicator %in% c("soh", "mot_soh"),
         period == "2021-01",
         !pill_count %in% c("30_count", "30")) %>% 
  group_by(product, indicator) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% prinf()

##sample looking at tld/tle400 over time
## create dfs
 
tld <- ppmr %>% 
  filter(regimen_optimized %in% c("TLD", "TLE 400"),
         indicator == "mot_soh") %>%
  separate(quarter, "fy", sep = "\\.", remove = FALSE) %>%
  group_by(country, period, quarter, fy) %>% 
  summarise(mot_soh = sum(value, na.rm = TRUE))


## MER targets
mer3 <- mer %>% 
  filter(indicator == "TX_CURR",
         trendscoarse == "15+") %>%
  rename(country = operatingunit) %>%
  reshape_msd("semi-wide") %>% 
  group_by(country, indicator, period) %>% 
  summarise(targets = sum(targets, na.rm = TRUE)) %>% 
  filter(targets != 0) %>% 
  rename(fy = period)

  # filter(str_detect(period, ("targets"))) %>%
  # separate(period, "fy", sep = "_", remove = FALSE) %>% 
  # group_by(country, period, fy) %>% 
  # summarise(tx_curr = sum(value, na.rm = TRUE)) %>% 
  # mutate(results_targets = if_else(str_detect(period, "targets"), "targets", "results"),
  #        mer_pd = period,
  #        period = case_when(mer_pd == "fy2019q3" ~ "2019-06",
  #                           mer_pd == "fy2019q4" ~ "2019-09",
  #                           mer_pd == "fy2020q1" ~ "2019-12",
  #                           mer_pd == "fy2020q2" ~ "2020-03",
  #                           mer_pd == "fy2020q3" ~ "2020-06",
  #                           mer_pd == "fy2020q4" ~ "2020-09",
  #                           mer_pd == "fy2021q1" ~ "2020-12")) %>% 
  # ungroup()

mer3 <- mer3 %>% 
  pivot_wider(names_from = results_targets, values_from = tx_curr) %>% 
  mutate(fy = str_remove(fy, "fy")) %>% 
  select(-period)

## create period in mer

#join and calculate ratio
df_all <- tld %>% 
  full_join(mer3, by = c("country", "fy")) %>%
  mutate(ratio = round(mot_soh/targets, 1)) %>% 
  group_by(fy) %>% 
  mutate(ratio2 = targets*2,
         ratio3 = targets*3)
  
  



df_all %>%
  filter(country %in% c("Botswana", "Zimbabwe", "Zambia")) %>% 
  ggplot(aes(x = period.x, y = mot_soh, group = country)) +
  geom_point(aes(fill = mot_soh),
             shape = 21, size = 5,
             ) +
  # geom_hline(data = . %>% filter(fy == "2019"),
  #                                aes(yintercept=ratio1), color = grey50k)
  geom_segment(data = . %>% filter(fy == "2019"),
               mapping = aes(x = "2019-08", y = targets, xend = "2019-12", yend = targets)) +
  geom_segment(data = . %>% filter(fy == "2020"),
               mapping = aes(x = "2020-01", y = targets, xend = "2020-12", yend = targets)) +
  geom_segment(data = . %>% filter(fy == "2021"),
               mapping = aes(x = "2020-12", y = targets, xend = "2021-01", yend = targets)) +
  facet_wrap(~country) +
  si_style()


#TE1
df_all %>%
  mutate(point_color = ifelse(mot_soh > targets, "#047491", "#af273d")) %>% 
  filter(country %in% c("Botswana" , "Mozambique", "Zimbabwe", "Zambia")) %>% 
  ggplot(aes(x = period.x, y = mot_soh, group = country)) +
  annotate("rect", xmin = "2020-03", xmax = "2021-01", ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
  # Put the lines under the points
  geom_segment(data = . %>% filter(fy == "2019"),
               mapping = aes(x = "2019-08", y = targets, xend = "2019-12", yend = targets),
               color = grey30k, size = 1) +
  geom_segment(data = . %>% filter(fy == "2020"),
               mapping = aes(x = "2019-12", y = targets, xend = "2020-12", yend = targets),
               color = grey30k, size = 1) +
  geom_segment(data = . %>% filter(fy == "2021"),
               mapping = aes(x = "2020-12", y = targets, xend = "2021-01", yend = targets),
               color = grey30k, size = 1
  ) +
  geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85)+
  geom_point(aes(fill = point_color),
             shape = 21, size = 7) + 
  geom_text(aes(y = mot_soh, label = ratio), size = 7/.pt, color = "white") +
  facet_wrap(~country, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_fill_identity() +
  si_style_ygrid() +
  coord_cartesian(clip = "off") 


#TE2
df_all %>%
  mutate(point_color = ifelse(mot_soh > targets, "#047491", "#af273d")) %>% 
  filter(country %in% c("Zambia", "Cameroon"),
         fy != "2019") %>% 
  ggplot(aes(x = period, y = ratio, group = country)) +
  annotate("rect", xmin = "2020-03", xmax = "2021-01", ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
  geom_hline(yintercept = 1, size = 1, linetype = "dotted", color = grey80k)+
  geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85) +
  geom_point(aes(fill = point_color),
             shape = 21, size = 8, stroke = 0.1) + 
  geom_text(aes(y = ratio, label = ratio), size = 9/.pt, color = "white") +
  scale_fill_identity() +
  new_scale_fill() +
  geom_tile(aes(fill = mot_soh, y = -1.5), color = "white") +
  geom_text(aes(label = paste0(comma((round(mot_soh/1000)), 1), "K"), y = -1.5), size = 7/.pt) +
  scale_fill_si(palette = "scooters", discrete = F, alpha = 0.75) +
  scale_y_continuous() +
  facet_wrap(~country) +
  si_style_ygrid() +
  coord_cartesian(clip = "on") +
  labs(x = NULL, y = NULL, title = "")+
  theme(legend.position = "none") +
  si_save("mot_soh_example.png", path = Dataout)


df_all %>%
  filter(country %in% c("Zambia", "Cameroon")) %>% 
  group_by(country, period) %>% 
  summarise(across(c(mot_soh, targets), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = country,
              values_from = mot_soh:targets)
  
  
  
  
  
  
  

