# PURPOSE: Munge and Analysis of PPRM and SC_FACT data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2021-10-6
# NOTES: Create SC_FACT + PPMR dataset
#     updated for 1/24 DPs meeting to include focus country filter

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    
    

    library(extrafont)
    library(scales)
    library(ggnewscale)

  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
     
    
  # Functions  
  

# LOAD DATA ============================================================================  
    
  #mer OU by IM
    mer_df <- read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY19-22_20211217_v2_1.zip"))
    
    # df_ppmr <- read_csv("Dataout/ppmr_processed_20211013.csv")
    df_ppmr <- ppmr_df()
    
    sc_fact <- read_csv("Dataout/sc_fact_processed_20211013.csv")
    
    ou1 <- c("Botswana", "Burundi", "Cameroon", "Cote d'Ivoire", "Democratic Republic of the Congo",
             "Eswatini", "Ethiopia", "Haiti", "Lesotho")
    
    ou2 <- c("Malawi", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Uganda",
             "Vietnam", "Zambia", "Zimbabwe")
    
    focus_ous <- c("Angola", "Cameroon", "Democratic Republic of the Congo", "Kenya", "Haiti", 
                                 "Mozambique", "Nigeria", "Rwanda", "South Sudan",
                                 "Tanzania", "Uganda", "Zambia", "Zimbabwe")


# MUNGE ============================================================================
  
    #create MER targets df
    mer_targ <- mer_df %>% 
      filter(indicator == "TX_CURR",
             trendscoarse == "15+",
             fiscal_year != 2022) %>%
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
                            fy == "FY21" ~ 2021))
    
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
    
    df_sc <- sc_fact %>% 
      filter(fiscal_year == 2021,
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
             !is.na(mot_soh),
             country %in% scfact_ous) %>% 
      mutate(ratio = round(mot_soh/TX_CURR, 1))
    
   

  # # #create country by period df for gt
  # #   
  # #   df_ppmr %>% 
  # #     filter(value != 0) %>%
  # #     group_by(country, period) %>%
  # #     summarise(value = sum(value)) %>%
  # #     arrange(period) %>% 
  # #     pivot_wider(names_from = period, values_from = value) %>% 
  # #     write_csv("Dataout/ou_period_10.1.21.csv")
  #   
  #   pd = case_when(period == "2019-08-31" ~ "8/19",
  #                  period == "2019-09-30" ~ "9/19",
  #                  period == "2019-10-31" ~ "10/19",
  #                  period == "2019-11-30" ~ "11/19",
  #                  period == "2019-12-31" ~ "12/19",
  #                  period == "2020-01-14" ~ "1/20",
  #                  period == "2020-01-31" ~ "1/20",
  #                  period == "2020-02-29" ~ "2/20",
  #                  period == "2020-03-31" ~ "3/20",
  #                  period == "2020-04-30" ~ "4/20",
  #                  period == "2020-05-31" ~ "5/20",
  #                  period == "2020-06-30" ~ "6/20",
  #                  period == "2020-07-31" ~ "7/20",
  #                  period == "2020-08-16" ~ "8/20",
  #                  period == "2020-08-31" ~ )) %>% 
    
# VIZ ============================================================================

    #TE2
      #Final, this works, break into two groups
      
      df_all %>%
        arrange(period) %>%
        mutate(point_color = ifelse(mot_soh > TX_CURR, "#047491", "#af273d"),
               date_sort = fct_reorder(pd, period, .desc = FALSE)) %>% 
        filter(fy == "2021",
               country %in% focus_ous,
               period != "2021-10-31") %>% 
        ggplot(aes(x = period,
                   y = ratio, group = country)) +
        annotate("rect", xmin = as.Date("2020-10-31"), xmax = as.Date("2021-07-31"), ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
        geom_hline(yintercept = 1, size = 1, linetype = "dotted", color = grey80k) +
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
        scale_x_date(date_breaks = "1 month", date_labels = "%b%y") +
        facet_wrap(~country) +
        si_style_ygrid() +
        coord_cartesian(clip = "on") +
        labs(x = NULL, y = NULL, title = "Ratio of TLD stock and treatment target",
             subtitle = "FY21 TX_CURR target compared to months of treatment of TLD stock",
             caption = "Source: PPMR-HIV 2021.10.1, FY21Q4c MSD")+
        theme(legend.position = "none") +
        si_save("Images/tld_ratio_focus.png", scale = 1.6) 
      
      #ou group 2
      
      df_all %>%
        arrange(period) %>%
        mutate(point_color = ifelse(mot_soh > TX_CURR, "#047491", "#af273d"),
               date_sort = fct_reorder(pd, period, .desc = FALSE)) %>% 
        filter(fy == "2021",
               country %in% ou2) %>% 
        ggplot(aes(x = period,
                   y = ratio, group = country)) +
        annotate("rect", xmin = as.Date("2020-10-31"), xmax = as.Date("2021-07-31"), ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
        geom_hline(yintercept = 1, size = 1, linetype = "dotted", color = grey80k) +
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
        scale_x_date(date_breaks = "1 month", date_labels = "%b%y") +
        facet_wrap(~country) +
        si_style_ygrid() +
        coord_cartesian(clip = "on") +
        labs(x = NULL, y = NULL, title = "Ratio of TLD stock and treatment target",
             subtitle = "FY21 TX_CURR target compared to months of treatment of TLD stock",
             caption = "Source: PPMR-HIV 2021.10.1, FY21Q3c MSD")+
        theme(legend.position = "none") +
        si_save("Images/tld_ratio_2.png", scale = 1.6) 
      
      
  ##same viz now for joint PPMR+SC_FACT
      
      joint %>%
        arrange(period) %>%
        mutate(point_color = ifelse(mot_soh > TX_CURR, "#047491", "#af273d"),
               date_sort = fct_reorder(pd, period, .desc = FALSE)) %>% 
        filter(fy == "2021") %>% 
        ggplot(aes(x = period,
                   y = ratio, group = country)) +
        annotate("rect", xmin = as.Date("2020-10-31"), xmax = as.Date("2021-07-31"), ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
        geom_hline(yintercept = 1, size = 1, linetype = "dotted", color = grey80k) +
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
        scale_x_date(date_breaks = "1 month", date_labels = "%b%y") +
        facet_wrap(~country) +
        si_style_ygrid() +
        coord_cartesian(clip = "on") +
        labs(x = NULL, y = NULL, title = "Ratio of TLD stock and treatment target",
             subtitle = "FY21 TX_CURR target compared to months of treatment of TLD stock",
             caption = "Source: PPMR-HIV 2021.10.1, FY21Q3c MSD")+
        theme(legend.position = "none") +
        si_save("Images/tld_ratio_joint.png", scale = 1.6) 
      
      

# SPINDOWN ============================================================================
      #first attempt
      df_all %>%
        filter(country %in% c("Botswana", "Zimbabwe", "Zambia")) %>% 
        ggplot(aes(x = period, y = mot_soh, group = country)) +
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
        mutate(point_color = ifelse(mot_soh > TX_CURR, "#047491", "#af273d")) %>% 
        filter(country %in% c("Zambia")) %>% 
        ggplot(aes(x = pd, y = mot_soh, group = country)) +
        annotate("rect", xmin = "2020-03", xmax = "2021-01", ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
        # Put the lines under the points
        geom_segment(data = . %>% filter(fy == "2019"),
                     mapping = aes(x = "2019-08", y = TX_CURR, xend = "2019-12", yend = TX_CURR),
                     color = grey30k, size = 1) +
        geom_segment(data = . %>% filter(fy == "2020"),
                     mapping = aes(x = "2019-12", y = TX_CURR, xend = "2020-12", yend = TX_CURR),
                     color = grey30k, size = 1) +
        geom_segment(data = . %>% filter(fy == "2021"),
                     mapping = aes(x = "2020-12", y = TX_CURR, xend = "2021-01", yend = TX_CURR),
                     color = grey30k, size = 1) +
        geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85)+
        geom_point(aes(fill = point_color),
                   shape = 21, size = 7) + 
        geom_text(aes(y = mot_soh, label = ratio), size = 7/.pt, color = "white") +
        facet_wrap(~country, scales = "free_y") +
        scale_y_continuous(labels = comma) +
        scale_fill_identity() +
        si_style_ygrid() +
        coord_cartesian(clip = "off") 
      
      # 1/21/22 update for foucs countries
      
      df_all <- read_csv("Dataout/trianguled_sch_mer.csv")
      
      df_viz <- df_all %>%
        arrange(period) %>%
        mutate(point_color = ifelse(mot_soh > TX_CURR, "#047491", "#af273d"),
               date_sort = fct_reorder(pd, period, .desc = FALSE)) %>% 
        filter(fy == "2021",
               country %in% focus_ous,
               period != "2021-10-31")
      
      #viz
      # df_viz %>% 
      #   ggplot(aes(x = period,
      #              y = ratio, group = country)) +
      #   annotate("rect", xmin = as.Date("2020-10-31"), xmax = as.Date("2021-07-31"), ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
      #   geom_hline(yintercept = 1, size = 1, linetype = "dotted", color = grey80k) +
      #   geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85) +
      #   geom_point(aes(fill = point_color),
      #              shape = 21, size = 8, stroke = 0.1) + 
      #   geom_text(aes(y = ratio, label = ratio), size = 9/.pt, color = "white") +
      #   scale_fill_identity() +
      #   new_scale_fill() +
      #   geom_tile(aes(fill = mot_soh, y = -1.5), color = "white") +
      #   geom_text(aes(label = paste0(comma((round(mot_soh/1000)), 1), "K"), y = -1.5), size = 7/.pt) +
      #   scale_fill_si(palette = "scooters", discrete = F, alpha = 0.75) +
      #   scale_y_continuous() +
      #   scale_x_date(date_breaks = "1 month", date_labels = "%b%y", limits = c(min(df_all$period), max(df_all$period))) +
      #   facet_wrap(~country) +
      #   si_style_ygrid() +
      #   coord_cartesian(clip = "on") +
      #   labs(x = NULL, y = NULL, title = "Ratio of TLD stock and treatment target",
      #        subtitle = "FY21 TX_CURR target compared to months of treatment of TLD stock",
      #        caption = "Source: PPMR-HIV 2021.10.1, FY21Q4c MSD")+
      #   theme(legend.position = "none") +
      #   si_save("Images/tld_ratio_focus.png", scale = 1.6)
      
      library(glitr)
      library(glamr)
      library(gisr)
      library(tidyverse)
      library(gophr)
      library(scales)
      library(sf)
      library(extrafont)
      library(tidytext)
      library(ggnewscale)
      
      df_all <- read_csv("Dataout/trianguled_sch_mer.csv")
      
      df_all %>% write_csv("Dataout/trianguled_sch_mer.csv")
      
      
      df_viz <- df_all %>% 
        arrange(period) %>%
        mutate(point_color = ifelse(mot_soh > TX_CURR, "#047491", "#af273d"),
               date_sort = fct_reorder(pd, period, .desc = FALSE),
               text_color = ifelse(mot_soh/1000 < 5500, grey90k, "#FFFFFF")) %>% 
        filter(fy == "2021",
               country %in% focus_ous,
               period != "2021-10-31") 
      
      
      min(df_viz$period)
      max(df_viz$period)
    
      
      df_viz %>%
        ggplot(aes(x = period,
                   y = ratio, group = country)) +
        annotate("rect", xmin = as.Date("2020-10-31"), xmax = as.Date("2021-07-31"), ymin = 0, ymax = Inf, alpha = 0.5, fill = grey10k) +
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
             subtitle = "FY21 TX_CURR target compared to months of treatment of TLD+TLE400 stock on hand at a national level",
             caption = "Source: PPMR-HIV 2021.10.1, FY21Q4c MSD")+
        theme(legend.position = "none") +
        si_save("Images/tld_ratio_focus_v2.png", scale = 1.5)
        
      
      
      
      
      
      
      
      
      
