# PURPOSE: Munge and Analysis of PPMR, SC_FACT, and MER
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-02-01
# NOTES: Create PPMR + MER + SC_FACT and the viz comparing those

# LOCALS & SETUP ============================================================================

  # Libraries
    oha

    library(extrafont)
    library(scales)
    library(ggnewscale)
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  

# LOAD DATA ============================================================================  

    #FY21q4 clean
    mer_df <- read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY19-22_20211217_v2_1.zip"))
    
    #2.1.21 ppmr
    # df_ppmr <- read_csv("Dataout/ppmr_processed_20211013.csv")
    df_ppmr <- ppmr_df()
    
    #2.1.21 SC_FACT
    df_scfact <- sc_fact_df()

    #little exploration/qc
    
    df_scfact %>% distinct(country)
    df_scfact %>% distinct(period) %>% arrange(period) %>% prinf()
    
    #check the join 
    df_scfact %>% distinct(indicator)
    df_scfact %>% 
      filter(indicator == "mot",
             value == 0)
    
# MUNGE ============================================================================
  
  # update mer munge to include FY22
    mer_targ <- mer_df %>% 
      filter(indicator == "TX_CURR",
             trendscoarse == "15+") %>%
      reshape_msd("semi-wide") %>%
      rename(country = operatingunit) %>% 
      group_by(country, indicator, period) %>% 
      summarise(targets = sum(targets, na.rm = TRUE)) %>%
      ungroup() %>% 
      filter(targets != 0) %>% 
      rename(fy = period) %>% 
      pivot_wider(names_from = indicator, values_from = targets) %>% 
      mutate(fy = case_when(fy == "FY19" ~ 2019,
                            fy == "FY20" ~ 2020,
                            fy == "FY21" ~ 2021,
                            fy == "FY22" ~ 2022))
    
    #create TLD dataset from PPMR
    
    #check country list
    df_ppmr %>% arrange(country) %>% distinct(country) %>% prinf()
    
    
    tld <- df_ppmr %>% 
      filter(regimen_optimized %in% c("TLD", "TLE 400"),
             indicator == "mot_soh") %>%
      separate(quarter, "fy", sep = "\\.", remove = FALSE) %>%
      mutate(fy = as.double(fy),
             month = lubridate::month(period),
             year =lubridate::year(period),
             country = case_when(country == "Drc" ~ "Democratic Republic of the Congo",
                                 country == "Cote d'ivoire" ~ "Cote d'Ivoire",
                                 country == "Nigeria-flare" ~ "Nigeria",
                                 country == "Uganda-jms" ~ "Uganda",
                                 country == "Uganda-maul" ~ "Uganda",
                                 country == "Uganda-nms" ~ "Uganda",
                                 TRUE ~ country)) %>% 
      unite("pd", month:year, sep = "-", remove = FALSE) %>%
      # mutate(pd = str_remove(pd, "20")) %>% 
      group_by(country, pd, period, quarter, fy) %>% 
      summarise(mot_soh = sum(value, na.rm = TRUE)) %>% 
      ungroup() 
    
    
    #create joined df and calculate ratio
    df_all <- tld %>% 
      full_join(mer_targ, by = c("country", "fy")) %>%
      mutate(ratio = round(mot_soh/TX_CURR, 1)) %>% 
      group_by(fy) %>% 
      mutate(ratio2 = TX_CURR*2,
             ratio3 = TX_CURR*3) %>% 
      filter(!is.na(ratio)) %>%
      ungroup()
    
    #create SC_fact dataset for 1) TLD 2) rolled up to OU
    
    df_sc <- df_scfact %>% 
      filter(fiscal_year %in% c(2021, 2022),
             indicator == "mot_soh",
             regimen_optimized %in% c("TLD", "TLE 400")) %>%
      mutate(fy = as.double(fiscal_year),
             month = lubridate::month(period),
             year =lubridate::year(period)) %>% 
      unite("pd", month:year, sep = "-", remove = FALSE) %>% 
      group_by(country, period, pd) %>% 
      summarise(mot_soh = sum(value, na.rm = TRUE)) %>% 
      ungroup()
    
    scfact_ous <- df_sc %>% distinct(country) %>% 
      pull()
    
    ##create ppmr + sc_fact
    joint <- tld %>%
      filter(fy == 2021) %>% 
      mutate(period = format(period, "%Y-%m"),
             period = as.Date(as.yearmon(period))) %>%
      select(-quarter, -fy) %>% 
      bind_rows(df_sc) %>%
      group_by(country, pd, period) %>% 
      summarise(mot_soh = sum(mot_soh)) %>%
      ungroup() %>% 
      full_join(mer_targ, by = c("country")) %>% 
      filter(fy == 2021,
             !is.na(mot_soh)) %>% 
      mutate(ratio = round(mot_soh/TX_CURR, 1))
  
# VIZ ============================================================================
    
  df_viz <- joint %>%
      filter(country == "Mozambique",
             fy == "2021",
             period != "2021-10-31",
             period != "2021-11-30") %>%
      mutate(point_color = ifelse(mot_soh > TX_CURR, "#047491", "#af273d"),
             date_sort = fct_reorder(pd, period, .desc = FALSE),
             text_color = ifelse(mot_soh/1000 < 5500, grey90k, "#FFFFFF"))
     
    min(df_viz$period)
    max(df_viz$period) 

  
    
    df_viz %>%
      ggplot(aes(x = period,
                 y = ratio, group = country)) +
      annotate("rect", xmin = as.Date("2020-10-31"), xmax = as.Date("2021-09-30"), ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
      geom_hline(yintercept = 1, size = 1, linetype = "dotted", color = grey80k) +
      geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85) +
      geom_point(aes(fill = point_color),
                 shape = 21, size = 8, stroke = 0.1) + 
      geom_text(aes(y = ratio, label = ratio), size = 9/.pt, color = "white") +
      scale_fill_identity() +
      new_scale_fill() +
      geom_tile(aes(fill = mot_soh, y = -1.5), color = "white") +
      # geom_text(aes(label = paste0(label_number_si()(mot_soh/1000)), y = -1.5, color = text_color), size = 7/.pt) +
      geom_text(aes(label = paste0(comma((round(mot_soh/1000)), 1), "K"), y = -1.5), size = 7/.pt) +
      scale_color_identity() +
      scale_fill_si(palette = "scooters", discrete = F, alpha = 0.75) +
      scale_y_continuous() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b%y", limits = c(min(df_viz$period), max(df_viz$period))) +
      facet_wrap(~country) +
      si_style_ygrid() +
      coord_cartesian(clip = "on") +
      labs(x = NULL, y = NULL, title = "Ratio of TLD+TLE400 stock and TX_CURR target",
           subtitle = "FY21 TX_CURR target compared to months of treatment of TLD+TLE400 stock on hand at sites + national level",
           caption = "Source: PPMR-HIV 2022.2.1, SC_FACT 2022.2.1,  FY21Q4c MSD")+
      theme(legend.position = "none") +
      si_save("Images/tld_ratio_moz_sc_ppmr.png", scale = 1.5)
    
    #write data
    joint %>% write_csv("Dataout/sc_fact_ppmr_mer_tld_2022.2.1.csv")

    
# SPINDOWN ============================================================================

